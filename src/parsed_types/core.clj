(ns parsed-types.core
  "Parse, don't validate, for Clojure.

  [[defparsed]] defines a parsed type, its draft, and the checks between them. [[parse]] runs the checks on a draft and
  returns a new value of the parsed type, or throws. A function that needs checked data takes the parsed type and
  nothing else:

    (pt/defparsed Transfer [user-id from-id to-id amount]
      (pure ::amount-positive \"The amount is a positive decimal.\"
            [amount]
            (and (decimal? amount) (pos? amount)))
      (external ::funds-available \"The source account holds the amount.\"
                [amount] [source ::source]
                (<= amount (:balance source)))
      (fetch ::source [from-id] accounts-by-id))

    (defn transfer! [transfer]
      (pt/let [{:keys [from-id to-id amount]} (pt/parsed Transfer transfer)]
        (move-money! from-id to-id amount)))

  A check receives exactly the fields in its argument vector, then the fetches it binds. It passes when it returns
  exactly `true`; anything else is the failure, reported under its name.

  A `pure` check reads only its arguments, and its pass is remembered for the draft. An `external` one reads the
  world, and runs on every call. Nothing fetches and no external check runs until every pure check in the tree
  passes.

  A field declared with `children` or `child` holds drafts, and one [[parse]] parses the whole tree.

  Drafts and parsed values are maps with fixed fields: reading a key that is not a field throws. A parsed value cannot
  change: to change one, take [[draft-of]] it, `assoc`, and [[parse]] again. `assoc` on a draft keeps the remembered
  passes of the checks that do not read the field it sets."
  (:refer-clojure :exclude [let])
  (:require
   [clojure.set :as set])
  (:import
   (clojure.lang Reflector)
   (com.google.common.collect MapMaker)
   (java.lang.reflect Field Modifier)
   (java.util Map)
   (java.util.concurrent ConcurrentMap)))

(set! *warn-on-reflection* true)

(declare check-let-binding!)

(defmacro let
  "`clojure.core/let`, with one more check: a binding whose expression is `(pt/parsed Type x)` and whose form
  destructures a map has its keys checked against `Type`'s fields when the code compiles. `Type` may be a sum, whose
  members must all have the key.

    (pt/let [{:keys [from-id to-id amount]} (pt/parsed Transfer transfer)]
      (move-money! from-id to-id amount))

  For those bindings `:keys` and `:as` are allowed, and `{local :field}` pairs; `:or`, `:strs`, `:syms`, qualified
  keys and vector destructuring are refused. Every other binding is an ordinary `let` binding. Write it qualified,
  `pt/let`: referring it would shadow `clojure.core/let`."
  [bindings & body]
  (doseq [[form init] (partition 2 bindings)]
    ;; only `(pt/parsed ...)` is checked; resolving first keeps this namespace's own `let`s cheap while it loads
    (clojure.core/let [head (when (seq? init) (first init))
                       v    (when (and (symbol? head) (not (contains? &env head))) (resolve head))]
      (when (and (var? v) (= 'parsed-types.core/parsed (symbol v)))
        (check-let-binding! form init))))
  `(clojure.core/let ~bindings ~@body))

(defprotocol Draft
  "A draft that [[parse]] turns into a parsed type. [[defparsed]] and [[defdraft-type]] implement it."
  (declaration [draft]
    "`{:parses-to ParsedType, :checks [check ...], :fetches [fetch ...], :children {field child}}`. Return the same
    `def`ed map every time. [[defparsed]] writes it for you.

    `:parses-to` is a class [[defparsed-type]] defined. Its fields must be the draft's fields, in the same order, and no
    other draft type may parse to it.

    A check is `{:name :doc :kind :reads :fetches :status-code :f}`. `:kind` is `:pure`, for a check that reads only
    its arguments, whose pass is remembered, or `:external`, for one that reads the world, which runs on every call.
    Only an external check may name `:fetches`. `:status-code` is the HTTP status when this check is the first to
    fail; the default is 400 for a pure check and 403 for an external one.

    A pure check's pass is remembered for the draft, so the fields it reads must be immutable values, and its `:f`
    must not read a global that can change, such as an atom. The check that an `:f` closes over no locals does not see
    globals. It also refuses `constantly`, `partial`, `comp`, `memoize` and `with-meta` on a fn, which close over
    their arguments: write a `fn`.

    A fetch is `{:name :reads :f}`. Its `:f` takes every distinct tuple of the fields it reads, across all the drafts
    with this declaration in the call, and returns a map from each tuple to what it fetched.

    A child is `{:kind :one|:many, :type ParsedType}`. The field holds nil or a draft that parses to `ParsedType`
    (`:one`), or a vector of them (`:many`)."))

(definterface ParsedValue)

(definterface DraftValue)

(defn final!
  "Throws: a parsed value cannot be changed. Called by the types [[defparsed-type]] defines."
  [parsed op]
  (throw (ex-info (str (.getSimpleName (class parsed)) " is a parsed value and cannot be changed with " op
                       ". Use draft-of, then assoc, then parse.")
                  {:status-code 500, :parsed (class parsed), :op op})))

(defn fixed!
  "Throws: a draft's fields are fixed. Called by the types [[defdraft-type]] defines."
  [draft op]
  (throw (ex-info (str (.getSimpleName (class draft)) " has fixed fields and cannot be changed with " op ".")
                  {:status-code 500, :draft (class draft), :op op})))

(defn no-field!
  "Throws: `x` has no field `k`. Called by the types [[defparsed-type]] and [[defdraft-type]] define, when a key that
  is not a field is read without a default."
  [x k]
  (throw (ex-info (str (.getSimpleName (class x)) " has no field " (pr-str k))
                  {:status-code 500, :type (class x), :key k, :fields (vec (keys x))})))

(declare assoc-draft)

(defn- map-type-methods
  "The interfaces and methods of a read-only map with exactly `fields`. Reading another key throws unless a default
  is given. `assoc-form`, over `this`, `k` and `v`, is what `assoc` does; `final-name` throws for any other change.
  Every function in these bodies is written qualified, so no field name can shadow it."
  [fields this k v assoc-form final-name]
  (clojure.core/let [nf      (gensym "not-found")
                     f       (gensym "f")
                     init    (gensym "init")
                     o       (gensym "o")
                     kws     (mapv keyword fields)
                     lookup  (fn [missing] `(case ~k ~@(mapcat (fn [fld] [(keyword fld) fld]) fields) ~missing))
                     entries `[~@(map (fn [fld] `(clojure.lang.MapEntry/create ~(keyword fld) ~fld)) fields)]]
    ;; not MapEquivalence: like a record, a value is equal only to a value of its own type, never to a plain map
    `[clojure.lang.IPersistentMap
      (~'assoc [~this ~k ~v] ~assoc-form)
      (~'assocEx [~this ~k ~v] (~final-name ~this "assocEx"))
      (~'without [~this ~k] (~final-name ~this "dissoc"))
      (~'containsKey [~this ~k] (clojure.core/contains? ~(set kws) ~k))
      (~'entryAt [~this ~k] (if (clojure.core/contains? ~(set kws) ~k)
                              (clojure.lang.MapEntry/create ~k (.valAt ~this ~k))
                              nil))
      (~'count [~this] ~(count fields))
      (~'cons [~this ~o] (clojure.core/reduce (fn [m# [k# v#]] (clojure.core/assoc m# k# v#))
                                              ~this
                                              (if (clojure.core/map? ~o) ~o (clojure.core/some-> ~o clojure.core/vector))))
      (~'empty [~this] {})
      (~'equiv [~this ~o] (clojure.core/and (clojure.core/identical? (clojure.core/class ~this) (clojure.core/class ~o))
                                             (clojure.lang.APersistentMap/mapEquals ~this ~o)))
      (~'seq [~this] (clojure.core/seq ~entries))
      (~'iterator [~this] (clojure.lang.SeqIterator. (clojure.core/seq ~entries)))
      (~'valAt [~this ~k] ~(lookup `(no-field! ~this ~k)))
      (~'valAt [~this ~k ~nf] ~(lookup nf))

      clojure.lang.IKVReduce
      (~'kvreduce [~this ~f ~init]
       (clojure.core/reduce (fn [acc# [k# v#]] (~f acc# k# v#)) ~init (clojure.core/seq ~entries)))

      clojure.lang.IHashEq
      (~'hasheq [~this] (clojure.lang.APersistentMap/mapHasheq ~this))

      clojure.lang.IFn
      (~'invoke [~this ~k] (.valAt ~this ~k))
      (~'invoke [~this ~k ~nf] (.valAt ~this ~k ~nf))
      (~'applyTo [~this args#] (clojure.lang.AFn/applyToHelper ~this args#))

      clojure.lang.IObj
      (~'meta [~this] nil)
      (~'withMeta [~this ~k] (~final-name ~this "with-meta"))

      java.util.Map
      (~'get [~this ~k] (.valAt ~this ~k nil))
      (~'size [~this] ~(count fields))
      (~'isEmpty [~this] ~(empty? fields))
      (~'containsValue [~this ~o] (clojure.core/boolean (clojure.core/some #(clojure.core/= ~o %) [~@fields])))
      (~'keySet [~this] ~(set kws))
      (~'values [~this] [~@fields])
      (~'entrySet [~this] (clojure.core/set ~entries))
      (~'put [~this ~k ~v] (throw (UnsupportedOperationException.)))
      (~'putAll [~this ~o] (throw (UnsupportedOperationException.)))
      (~'remove [~this ~k] (throw (UnsupportedOperationException.)))
      (~'clear [~this] (throw (UnsupportedOperationException.)))

      Object
      (~'hashCode [~this] (clojure.lang.APersistentMap/mapHash ~this))
      (~'equals [~this ~o] (clojure.core/and (clojure.core/identical? (clojure.core/class ~this) (clojure.core/class ~o))
                                              (clojure.lang.APersistentMap/mapEquals ~this ~o)))
      (~'toString [~this] (clojure.core/str (clojure.core/into {} ~this)))]))

(defn- check-field-names!
  "Refuse fields that cannot be a type's fields: anything but simple symbols, a name twice, or a special form, which
  a field cannot shadow."
  [form fields]
  (when-not (and (vector? fields) (every? simple-symbol? fields))
    (throw (ex-info (str "fields must be a vector of simple symbols: " (pr-str form)) {:form form})))
  (when-not (apply distinct? ::sentinel fields)
    (throw (ex-info (str "a field is named twice: " (pr-str form)) {:form form})))
  (when-let [special (seq (filter #(or (special-symbol? %) (= '& %)) fields))]
    (throw (ex-info (str "a field cannot be named like a special form: " (vec special)) {:form form}))))

(defn- print-method-form [name]
  ;; `#ns.Type{...}`, like a record, so a value is told apart from a plain map at the REPL. The reader cannot build one
  ;; back: a map type is not a record.
  `(defmethod print-method ~name [x# ^java.io.Writer w#]
     (.write w# (str "#" (.getName (class x#))))
     (print-method (into {} x#) w#)))

(defmacro defparsed-type
  "Define `name`, a bare parsed type with `fields`: a map that only [[parse]] issues. `assoc`, `dissoc` and `with-meta`
  throw, and so does reading a key that is not a field. [[defparsed]] calls this; use it directly only with a
  declaration written by hand."
  [name fields]
  (check-field-names! &form fields)
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v")]
    `(do
       (deftype ~name [~@fields]
         parsed_types.core.ParsedValue
         ~@(map-type-methods fields this k v `(final! ~this "assoc") `final!))
       ~(print-method-form name)
       ~name)))

(defmacro defdraft-type
  "Define `name`, a bare draft type with `fields` and the declaration `declaration-expr`, evaluated with the fields in
  scope. `assoc` sets a field and keeps the remembered passes of the checks that do not read it; `dissoc` and
  `with-meta` throw, and so do reading or setting a key that is not a field. [[defparsed]] calls this; use it
  directly only with a declaration written by hand."
  [name fields declaration-expr]
  (check-field-names! &form fields)
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v")]
    `(do
       (deftype ~name [~@fields]
         parsed_types.core.DraftValue
         Draft
         (~'declaration [~this] ~declaration-expr)
         ~@(map-type-methods fields this k v `(assoc-draft ~this ~k ~v) `fixed!))
       ~(print-method-form name)
       ~name)))

;;; ------------------------------------------------ Sums ------------------------------------------------

(defn- parsed-type? [x]
  (and (class? x) (.isAssignableFrom ParsedValue ^Class x)))

(defrecord Sum [name members])

(defn- sum? [x] (instance? Sum x))

(defn- member?
  "Whether `cls` is `type`, or one of the members of the sum `type`."
  [type cls]
  (if (sum? type) (boolean (some #(identical? % cls) (:members type))) (identical? type cls)))

(defn- type-name [type]
  (if (sum? type) (str (:name type)) (.getSimpleName ^Class type)))

(defn sum
  "A sum of parsed types, named `name`. Use [[defsum]]."
  [name members]
  (when-not (and (seq members) (every? parsed-type? members) (apply distinct? members))
    (throw (ex-info (str "defsum " name ": members must be distinct types defined with defparsed")
                    {:name name, :members members})))
  (->Sum name (vec members)))

(defmacro defsum
  "Define `name`, a closed set of parsed types. [[match]] on it must handle every member, and a child field of a sum
  type holds drafts that parse to any member.

    (defsum Payment [Transfer Payroll])"
  [name members]
  `(def ~name (sum '~name ~members)))

(defmacro match
  "Branch on which member of the sum `sum-name` the parsed value `x` is. Each clause is a member type, a binding, and
  an expression:

    (match Payment payment
      Transfer t (transfer! t)
      Payroll  p (run-payroll! p))

  Leaving out a member, naming a type that is not one, or naming one twice is a compile error. Each binding is hinted
  with its type, and holds `x` only after [[parsed]] accepts it."
  [sum-name x & clauses]
  (let [s       (let [v (resolve sum-name)] (when (var? v) @v))
        _       (when-not (sum? s) (throw (ex-info (str "match: " sum-name " is not a sum defined with defsum") {})))
        _       (when-not (zero? (mod (count clauses) 3))
                  (throw (ex-info "match: each clause is a type, a binding, and an expression" {:clauses clauses})))
        triples (partition 3 clauses)
        types   (map (fn [[t]] (let [c (resolve t)]
                                 (when-not (class? c) (throw (ex-info (str "match: " t " is not a type") {})))
                                 c))
                     triples)
        members (set (:members s))
        missing (remove (set types) (:members s))
        extra   (remove members types)]
    (when (seq missing)
      (throw (ex-info (str "match on " sum-name " does not handle " (mapv #(.getSimpleName ^Class %) missing)) {})))
    (when (seq extra)
      (throw (ex-info (str "match on " sum-name ": not members: " (mapv #(.getSimpleName ^Class %) extra)) {})))
    (when-not (apply distinct? types)
      (throw (ex-info (str "match on " sum-name " handles a type twice") {})))
    (let [v (gensym "v")]
      `(let [~v ~x]
         (cond
           ~@(mapcat (fn [[t b e]]
                       [`(instance? ~t ~v) `(let [~(vary-meta b assoc :tag t) (parsed ~t ~v)] ~e)])
                     triples)
           :else (throw (ex-info (str "Expected a parsed member of " '~sum-name)
                                 {:status-code 500, :expected '~sum-name, :got (type ~v)})))))))

;;; ------------------------------------------------ defparsed ------------------------------------------------

(defn- syntax-error! [form message]
  (throw (ex-info (str "defparsed: " message ": " (pr-str form)) {:form form})))

(defn- parse-check-clause
  "`(pure name doc opts? [field ...] body ...)` or `(external name doc opts? [field ...] [local fetch ...]? body ...)`"
  [fields fetch-names [head n doc & more :as form]]
  (let [external?      (= 'external head)
        [opts more]    (if (map? (first more)) [(first more) (rest more)] [{} more])
        [args & more]  more
        _              (when (and external? (vector? (first more)) (nil? (next more))
                                  (seq (first more)) (every? keyword? (take-nth 2 (rest (first more)))))
                         (syntax-error! form "a check needs a body after its fetch bindings"))
        [binds body]   (if (and external? (vector? (first more)) (next more))
                         [(first more) (rest more)]
                         [[] more])
        pairs          (partition-all 2 binds)]
    (when-not (qualified-keyword? n) (syntax-error! form "a check's name must be a qualified keyword"))
    (when-not (string? doc) (syntax-error! form "a check needs a doc string after its name"))
    (when-let [unknown (seq (remove #{:status} (keys opts)))]
      (syntax-error! form (str "unknown options " (vec unknown) "; the only option is :status")))
    (when-not (or (nil? (:status opts)) (int? (:status opts)))
      (syntax-error! form ":status must be an integer"))
    (when (and (not external?) (vector? (first more)) (next more))
      (syntax-error! form "a pure check cannot fetch; make it external"))
    (when-not (and (vector? args) (every? symbol? args))
      (syntax-error! form "a check needs a vector of the fields it reads"))
    (when-let [unknown (seq (remove (set fields) args))]
      (syntax-error! form (str "a check reads fields the draft does not have: " (vec unknown))))
    (when-not (apply distinct? ::sentinel args) (syntax-error! form "a check reads a field twice"))
    (when-not (every? #(and (= 2 (count %)) (symbol? (first %)) (contains? fetch-names (second %))) pairs)
      (syntax-error! form "a check's fetches are pairs of a local and a fetch declared in this defparsed"))
    (when-not (apply distinct? ::sentinel (map first pairs))
      (syntax-error! form "a check binds two fetches to one local"))
    (when-let [clash (seq (filter (set fields) (map first pairs)))]
      (syntax-error! form (str "a fetch is bound to a field's name: " (vec clash) "; pick another local")))
    (when (empty? body) (syntax-error! form "a check needs a body"))
    ;; An unnamed field is usually an unresolved symbol, but a field named like a clojure.core fn (`name`, `count`)
    ;; would resolve to that fn instead, so look for every field by name.
    (when-let [unnamed (seq (filter (set (remove (set args) fields)) (filter symbol? (tree-seq coll? seq body))))]
      (syntax-error! form (str "a check uses fields its argument vector does not name: " (vec (distinct unnamed)))))
    (merge {:name    n
            :doc     doc
            :kind    (if external? :external :pure)
            :reads   (mapv keyword args)
            :f       `(fn [~@args ~@(map first pairs)] ~@body)}
           (when (seq pairs) {:fetches (mapv second pairs)})
           (when-let [s (:status opts)] {:status-code s}))))

(defn- parse-fetch-clause [fields [_ n reads f :as form]]
  (when-not (qualified-keyword? n) (syntax-error! form "a fetch's name must be a qualified keyword"))
  (when-not (and (vector? reads) (every? (set fields) reads))
    (syntax-error! form "a fetch needs a vector of fields of the draft"))
  (when-not f (syntax-error! form "a fetch needs a function of the tuples it fetches"))
  {:name n, :reads (mapv keyword reads), :f f})

(defmacro defparsed
  "Define `parsed-name`, a parsed type with `fields`, its draft type `<parsed-name>Draft` with the same fields, and the
  checks [[parse]] runs to turn a draft into a parsed value. Build a draft with `->NameDraft` or [[draft]].

    (defparsed Payroll [user-id from-id transfers]
      (children transfers Transfer)
      (pure ::one-source \"Every transfer draws on the payroll's account.\"
            [from-id transfers]
            (every? #(= from-id (:from-id %)) transfers))
      (external ::covers-total \"The account holds the sum of the transfers.\"
                [transfers] [source ::source]
                (<= (reduce + 0M (map :amount transfers)) (:balance source)))
      (fetch ::source [from-id] accounts-by-id))

  Clauses:

  - `(pure name doc opts? [field ...] body ...)`: a check over the named fields.
  - `(external name doc opts? [field ...] [local fetch-name ...]? body ...)`: a check over the named fields and the
    results of the named fetches.
  - `(fetch name [field ...] f)`: `f` takes every distinct tuple of the fields, and returns a map from each tuple to
    what it fetched.
  - `(children field ParsedType)`: the field holds a vector of drafts that parse to `ParsedType`.
  - `(child field ParsedType)`: the field holds nil or a draft that parses to `ParsedType`.

  `opts` is `{:status code}`. A check's argument vector is the fields it reads: one that is not a field is a compile
  error. Defines `<parsed-name>Draft-declaration`, private, holding the declaration."
  [parsed-name fields & clauses]
  (when-not (simple-symbol? parsed-name) (syntax-error! &form "write (defparsed Name [fields] clauses ...)"))
  (when-not (and (vector? fields) (every? simple-symbol? fields)) (syntax-error! &form "fields must be a vector of symbols"))
  (when-not (apply distinct? ::sentinel fields) (syntax-error! &form "a field is named twice"))
  (doseq [c clauses]
    (when-not (and (seq? c) (symbol? (first c)) ('#{pure external fetch child children} (symbol (name (first c)))))
      (syntax-error! c "a clause is (pure ...), (external ...), (fetch ...), (child ...) or (children ...)")))
  (let [clauses     (map #(cons (symbol (name (first %))) (rest %)) clauses)
        by-head     (group-by first clauses)
        fetches     (mapv #(parse-fetch-clause fields %) (get by-head 'fetch))
        fetch-names (set (map :name fetches))
        checks      (mapv #(parse-check-clause fields fetch-names %)
                          (filter (comp '#{pure external} first) clauses))
        children    (into {}
                          (for [[head field type :as c] (concat (get by-head 'child) (get by-head 'children))]
                            (do (when-not (contains? (set fields) field)
                                  (syntax-error! c "a child field must be a field of the draft"))
                                [(keyword field) {:kind (if (= 'child head) :one :many), :type type}])))
        child-heads (map second (concat (get by-head 'child) (get by-head 'children)))
        _           (when-not (apply distinct? ::sentinel child-heads)
                      (syntax-error! &form "a field is declared as a child twice"))
        names       (concat (map :name checks) (map :name fetches))
        draft-name  (symbol (str parsed-name "Draft"))
        decl-sym    (symbol (str draft-name "-declaration"))]
    (when-not (apply distinct? ::sentinel names) (syntax-error! &form "a check or fetch name is used twice"))
    (when-let [reserved (seq (filter #(= "parsed-types.child" (namespace %)) names))]
      (syntax-error! &form (str "names in parsed-types.child are reserved for child-field checks: " (vec reserved))))
    `(do
       (defparsed-type ~parsed-name ~fields)
       (def ~(vary-meta decl-sym assoc :private true)
         {:parses-to ~parsed-name
          :checks    ~checks
          :fetches   ~fetches
          :children  ~children})
       (defdraft-type ~draft-name ~fields ~decl-sym)
       (register-draft! ~parsed-name ~draft-name)
       ~parsed-name)))

;;; ------------------------------------------------ State ------------------------------------------------

;; Every value [[parse]] issued -> the plan of the draft it came from, by identity: Guava's weak keys compare with
;; `identical?`, not `=`, so an equal value built with the constructor is not in here. A secret in a field or in
;; metadata would not do: either can be read out of one parsed value and put into a forged one. Weak, so a parsed value
;; is garbage collected like any other.
(defonce ^:private ^Map issued
  (.makeMap (.weakKeys (MapMaker.))))

;; Draft -> `[plan mask]`: the pure checks it passed, one bit per check of that plan, by identity. Out of the draft so
;; it cannot be forged, and lost on a plain `assoc`, which makes every check run again. The plan is kept so the bits
;; are read only against the plan that set them.
(defonce ^:private ^Map passed
  (.makeMap (.weakKeys (MapMaker.))))

;; Parsed type -> the name of the one draft class that may parse to it. By name, not identity, so re-evaluating a
;; `defrecord` at a REPL does not lock its parsed type away; another class with that name can only come from the same
;; namespace.
(defonce ^:private ^ConcurrentMap claims
  (.makeMap (.weakKeys (MapMaker.))))

;; Declaration -> draft class -> plan, both by identity. Keyed by both, so after a REPL re-evaluates a draft type, a
;; draft of the old class that reads the new declaration cannot claim the new declaration's plan for itself.
(defonce ^:private ^ConcurrentMap plans
  (.makeMap (.weakKeys (MapMaker.))))

;; Parsed type -> its draft type, for [[draft]]. Written by [[defparsed]].
(defonce ^:private ^ConcurrentMap drafts-by-parsed-type
  (.makeMap (.weakKeys (MapMaker.))))

(defn register-draft!
  "Record that `draft-type` is the draft of `parsed-type`. Called by [[defparsed]]."
  [parsed-type draft-type]
  (.put drafts-by-parsed-type parsed-type draft-type))

;;; ------------------------------------------------ Plans ------------------------------------------------

(declare plan-for)

(defn- bad-declaration! [draft-class message data]
  (throw (ex-info (str (.getSimpleName ^Class draft-class) ": " message)
                  (merge {:status-code 500, :draft draft-class} data))))

(defn- closed-over-locals
  "The locals `f` closes over. A check must close over none, or what it reads would not be what it declares."
  [f]
  (into []
        (comp (remove #(Modifier/isStatic (.getModifiers ^Field %)))
              (map #(.getName ^Field %)))
        (.getDeclaredFields (class f))))

(defn- basis [^Class cls]
  (mapv keyword (Reflector/invokeStaticMethod cls "getBasis" (object-array 0))))

(defn- draft-value? [x]
  (and (instance? DraftValue x) (satisfies? Draft x)))

(defn- validate-declaration! [draft-class {:keys [parses-to checks fetches children]}]
  (let [fields      (set (basis draft-class))
        fetch-names (into #{} (map :name) fetches)
        names       (concat (map :name checks) (map :name fetches))]
    (when-not (parsed-type? parses-to)
      (bad-declaration! draft-class ":parses-to must be a type defined with defparsed" {:parses-to parses-to}))
    (when-not (= (basis draft-class) (basis parses-to))
      (bad-declaration! draft-class "the parsed type's fields must be the draft's fields, in order"
                        {:draft-fields (basis draft-class), :parsed-fields (basis parses-to)}))
    (when-let [dupes (seq (for [[n c] (frequencies names) :when (< 1 c)] n))]
      (bad-declaration! draft-class "a check or fetch name is used twice" {:names dupes}))
    (when-let [reserved (seq (filter #(and (keyword? %) (= "parsed-types.child" (namespace %))) names))]
      (bad-declaration! draft-class "names in parsed-types.child are reserved for child-field checks"
                        {:names reserved}))
    (doseq [[field {:keys [kind type]}] children]
      (when-not (contains? fields field)
        (bad-declaration! draft-class "a child field is not a field of the draft" {:field field}))
      (when-not (and (#{:one :many} kind) (or (parsed-type? type) (sum? type)))
        (bad-declaration! draft-class "a child is {:kind :one|:many, :type ParsedType-or-sum}" {:field field})))
    (doseq [{n :name, :keys [doc kind reads f], fs :fetches} (concat checks fetches)
            :let [check? (contains? (set (map :name checks)) n)]]
      (when-not (and (or (nil? reads) (sequential? reads))
                     (every? keyword? reads)
                     (= (count reads) (count (set reads))))
        (bad-declaration! draft-class ":reads must be a vector of distinct field keywords" {:name n, :reads reads}))
      (when-not (qualified-keyword? n)
        (bad-declaration! draft-class "a check or fetch name must be a qualified keyword" {:name n}))
      (when-not (fn? f)
        (bad-declaration! draft-class "a check or fetch needs an :f that is a fn" {:name n}))
      (when (and check? (not (string? doc)))
        (bad-declaration! draft-class "a check needs a :doc" {:name n}))
      (when (and check? (not (#{:pure :external} kind)))
        (bad-declaration! draft-class "a check's :kind is :pure or :external" {:name n, :kind kind}))
      (when (and check? (= :pure kind) (seq fs))
        (bad-declaration! draft-class "a :pure check cannot fetch" {:name n}))
      (when-let [unknown (seq (remove fields reads))]
        (bad-declaration! draft-class "a check or fetch reads a field the draft does not have"
                          {:name n, :unknown-fields unknown, :fields fields}))
      (when-let [unknown (seq (remove fetch-names fs))]
        (bad-declaration! draft-class "a check names a fetch that is not declared" {:name n, :unknown-fetches unknown}))
      (when-let [locals (and check? (= :pure kind) (seq (closed-over-locals f)))]
        (bad-declaration! draft-class (str "a pure check's :f closes over locals. It must read only its arguments, so"
                                           " that what it reads is what it declares, and its pass can be remembered.")
                          {:name n, :locals locals})))))

(defn- children-of-type?
  "Whether `v` is what a child field of `kind` and `parsed-type` may hold."
  [kind parsed-type v]
  (let [drafts-it? (fn [d] (and (draft-value? d) (member? parsed-type (:parses-to (plan-for d)))))]
    (case kind
      :one  (or (nil? v) (drafts-it? v))
      :many (and (vector? v) (every? drafts-it? v)))))

(defn- child-shape-check
  "The pure check every child field gets: it holds drafts of the declared type. Every check that reads the field waits
  on it."
  [field {:keys [kind type]}]
  {:name  (keyword "parsed-types.child" (name field))
   :doc   (str (name field) " holds " (if (= :one kind) "nil or a draft that parses to " "a vector of drafts that parse to ")
               (type-name type) ".")
   :kind  :pure
   :shape? true
   :reads [field]
   :f     (fn [v] (children-of-type? kind type v))})

(defn- reads-set [x] (set (:reads x)))

(defn- compile-plan
  "Order the checks, and work out what each waits on.

  - A pure check waits on every pure check that reads a proper subset of its fields: a check on one field (its shape)
    runs before a check relating it to another. A check that reads a child field also waits on that field's shape
    check, and on the children's own pure checks.
  - A fetch waits on every pure check, so malformed input never reads the world.
  - An external check waits on its fetches and on every pure check.

  Pure checks run first, fewest fields first, then fetches, then external checks, each in declaration order."
  [draft-class {:keys [parses-to checks fetches children], :as decl}]
  (validate-declaration! draft-class decl)
  (let [claimed (.putIfAbsent claims parses-to (.getName ^Class draft-class))]
    (when (and claimed (not= claimed (.getName ^Class draft-class)))
      (bad-declaration! draft-class (str (.getSimpleName ^Class parses-to) " is parsed from another draft type."
                                         " One draft type owns each parsed type, so a parsed value means its checks"
                                         " passed.")
                        {:parses-to parses-to, :owner claimed})))
  (let [shape     (into {} (map (fn [[field child]] [field (child-shape-check field child)])) children)
        pure      (into (vec (vals shape)) (filter #(= :pure (:kind %))) checks)
        _         (when (< 64 (count pure))
                    (bad-declaration! draft-class "more than 64 pure checks, counting one per child field" {}))
        external  (filterv #(= :external (:kind %)) checks)
        bit       (zipmap (map :name pure) (range))
        waits-on  (fn [pred] (into #{} (comp (filter pred) (map :name)) pure))
        all-pure  (waits-on any?)
        shapes-of (fn [c] (into #{} (keep #(:name (get shape %))) (:reads c)))
        pure'     (for [c pure
                        :let [rs (reads-set c)]]
                    (assoc c
                           :pure?   true
                           :bit     (bit (:name c))
                           :prereqs (-> (waits-on #(and (not= (:name %) (:name c))
                                                        (set/subset? (reads-set %) rs)
                                                        (not= (reads-set %) rs)))
                                        (into (shapes-of c))
                                        (disj (:name c)))))
        fetches'  (for [fd fetches]
                    (assoc fd :prereqs all-pure))
        external' (for [c external]
                    (assoc c :prereqs (into all-pure (:fetches c))))]
    {:draft-class     draft-class
     :parses-to       parses-to
     :fields          (basis draft-class)
     :children        children
     :pure            (vec (sort-by (comp count reads-set) pure'))
     :fetches         (vec fetches')
     :external        (vec external')
     :checks-by-name  (into {} (map (juxt :name identity)) (concat pure' external'))
     :fetches-by-name (into {} (map (juxt :name identity)) fetches')
     :all-pure-mask   (reduce #(bit-set %1 (:bit %2)) 0 pure')
     :field-mask      (reduce (fn [m {:keys [reads], b :bit}]
                                (reduce #(update %1 %2 (fnil bit-or 0) (bit-set 0 b)) m reads))
                              {}
                              pure')}))

(defn- plan-for [draft]
  (when-not (draft-value? draft)
    (throw (ex-info (str "Not a draft defined with defparsed or defdraft-type: " (type draft))
                    {:status-code 500, :got (type draft)})))
  (let [decl     (declaration draft)
        cls      (class draft)
        by-class (or (.get plans decl)
                     (let [m (.makeMap (.weakKeys (MapMaker.)))]
                       (or (.putIfAbsent plans decl m) m)))]
    (or (.get ^ConcurrentMap by-class cls)
        ;; two threads may compile the same plan; both keep the one that won, so neither loses its remembered passes
        (let [plan (compile-plan cls decl)]
          (or (.putIfAbsent ^ConcurrentMap by-class cls plan) plan)))))

;;; ------------------------------------------------ Running ------------------------------------------------

(defn- needed
  "`names` and everything they wait on, transitively."
  [plan names]
  (loop [todo (vec names), seen #{}]
    (if-let [[n & more] (seq todo)]
      (if (seen n)
        (recur (vec more) seen)
        (let [x (or ((:checks-by-name plan) n)
                    ((:fetches-by-name plan) n)
                    (throw (ex-info (str "Unknown check " n) {:status-code 500
                                                              :draft       (:draft-class plan)
                                                              :check       n})))]
          (recur (into (vec more) (:prereqs x)) (conj seen n))))
      seen)))

(defn- remembered-mask
  "The pure checks `draft` is known to pass under `plan`, one bit each."
  [draft plan]
  (let [[remembered-plan mask] (.get passed draft)]
    (if (identical? plan remembered-plan) (long mask) 0)))

(defn- read-fields
  "The draft's field values, read once from its Java fields, not through `get`, which a type could make answer
  differently on each call."
  [draft fields]
  (into {} (map (fn [f] [f (Reflector/getInstanceField draft (munge (name f)))])) fields))

(defn- start [draft names]
  (let [plan (plan-for draft)]
    {:draft   draft
     ;; every field read once, so the checks and the parsed value see the same values
     :values  (read-fields draft (:fields plan))
     :plan    plan
     :needed  (needed plan (or names (keys (:checks-by-name plan))))
     :mask    (remembered-mask draft plan)
     :fetched {}
     :failed  {}     ; check name -> failure entry
     :order   []     ; failed check names, in the order they failed
     :roots   {}}))  ; blocked check or fetch name -> the failed checks that blocked it

(defn- child-path [path field i]
  (if i (conj path field i) (conj path field)))

(def ^:private max-depth
  "The most levels a tree [[parse]] takes may have, counting the root. A deeper one is refused with a 400."
  1000)

(def ^:private max-nodes
  "The most drafts one tree may hold, counting a draft each time it appears."
  100000)

(defn- too-big! [message]
  (throw (ex-info message {:status-code 400, :max-depth max-depth, :max-nodes max-nodes})))

(defn- flatten-tree
  "The drafts of the tree under `root`, children before parents, each with its `:path` from the root. `names` applies
  to the root; every child runs all its checks."
  [tree root names]
  (loop [stack (list [root [] names 1]), pre []]
    (if-let [[draft path nm depth] (first stack)]
      (let [_    (when (< max-depth depth) (too-big! "Tree too deep"))
            _    (when (<= max-nodes (count pre)) (too-big! "Tree too large"))
            st   (assoc (start draft nm) :tree tree :path path)
            kids (for [[field {:keys [kind type]}] (get-in st [:plan :children])
                       :let  [v (get-in st [:values field])]
                       :when (children-of-type? kind type v)
                       [i d] (if (= :one kind) (when v [[nil v]]) (map-indexed vector v))]
                   [field (child-path path field i) d])
            st   (assoc st :child-paths (reduce (fn [m [field p]] (update m field (fnil conj []) p)) {} kids))]
        ;; pushing the children in order pops the last first, so the reverse of this pre-order is a post-order with
        ;; the children in order
        (recur (into (rest stack) (map (fn [[_ p d]] [d p nil (inc depth)])) kids) (conj pre st)))
      (vec (rseq pre)))))

(defn- roots-of
  "The failed checks behind `prereqs`, or nil when none failed."
  [st prereqs]
  (seq (distinct (mapcat #(if (contains? (:failed st) %) [%] (get-in st [:roots %])) prereqs))))

(defn- block [st n roots check?]
  (cond-> (assoc-in st [:roots n] roots)
    check? (as-> st (reduce #(update-in %1 [:failed %2 :blocked] (fnil conj []) n) st roots))))

(defn- run-check [st {n :name, :keys [prereqs f doc reads fetches], :as c}]
  (if-let [roots (roots-of st prereqs)]
    (block st n roots true)
    ;; a shape check always runs: a child's declaration can change under a remembered pass, at a REPL
    (let [remembered? (and (:pure? c) (not (:shape? c)) (bit-test (:mask st) (:bit c)))
          result      (if remembered?
                        true
                        (apply f (concat (map #(get (if (:shape? c) (:values st) (:inputs st)) %) reads)
                                         (map #(get-in st [:fetched %]) fetches))))]
      (cond
        (not (true? result))
        (-> st
            (assoc-in [:failed n] {:doc doc, :returned result})
            (update :order conj n))

        (:pure? c)
        (update st :mask bit-set (:bit c))

        :else
        st))))

(defn- failed-children
  "The paths of the children in the fields `c` reads whose own pure checks did not all pass. A shape check reads only
  the field's form, so it waits on no child."
  [st c bad]
  (when-not (:shape? c)
    (seq (for [field (:reads c)
               path  (get-in st [:child-paths field])
               :when (bad [(:tree st) path])]
           path))))

(defn- run-pure [st bad]
  (reduce (fn [st {n :name, :keys [doc prereqs], :as c}]
            (if-let [paths (and (not (roots-of st prereqs)) (failed-children st c bad))]
              ;; held, not run: a check over children that failed would read malformed values
              (-> st
                  (assoc-in [:failed n] {:doc doc, :blocked-by (vec paths)})
                  (update :order conj n))
              (run-check st c)))
          st
          (filter (comp (:needed st) :name) (get-in st [:plan :pure]))))

(declare construct)

(defn- pure-phase
  "Run every node's pure checks, children before parents, and build each node's value: a value of its parsed type,
  holding its children's values. A parent's checks see each child as that value, which is closed like any parsed
  value, and is the one [[parse]] issues if the tree passes. A node is bad when it, or anything under it, failed."
  [nodes]
  (first (reduce (fn [[out bad views] {:keys [tree path child-paths plan values], :as st}]
                   (let [inputs (reduce-kv (fn [m field paths]
                                             (assoc m field (if (= :one (get-in plan [:children field :kind]))
                                                              (views [tree (first paths)])
                                                              (mapv #(views [tree %]) paths))))
                                           values child-paths)
                         view   (construct (:parses-to plan) (:fields plan) inputs)
                         st     (run-pure (assoc st :inputs inputs, :view view) bad)
                         bad?   (or (seq (:failed st)) (some #(bad [tree %]) (apply concat (vals child-paths))))]
                     [(conj out st) (cond-> bad bad? (conj [tree path])) (assoc views [tree path] view)]))
                 [[] #{} {}]
                 nodes)))

(defn- run-external [st]
  (reduce run-check st (filter (comp (:needed st) :name) (get-in st [:plan :external]))))

(defn- fetch-phase
  "Run every fetch the nodes need, each once per plan across all of them, with each distinct tuple of arguments once."
  [sts]
  (let [wanted (for [[i st] (map-indexed vector sts)
                     :when  (:world? st)
                     fd     (get-in st [:plan :fetches])
                     :when  ((:needed st) (:name fd))]
                 {:i i, :fd fd, :roots (roots-of st (:prereqs fd))})
        sts    (reduce (fn [sts {:keys [i fd roots]}]
                         (cond-> sts roots (update i block (:name fd) roots false)))
                       sts wanted)
        tuple  (fn [{:keys [i fd]}] (mapv #(get (:inputs (nth sts i)) %) (:reads fd)))]
    (reduce
     (fn [sts [_ ws]]
       (let [{fd :fd} (first ws)
             tuples   (into [] (comp (map tuple) (distinct)) ws)
             result   ((:f fd) tuples)]
         (when-let [missing (seq (remove #(and (map? result) (contains? result %)) tuples))]
           (throw (ex-info (str "Fetch " (:name fd) " returned no entry for some of its arguments")
                           {:status-code 500, :fetch (:name fd), :missing missing})))
         (reduce (fn [sts w]
                   (assoc-in sts [(:i w) :fetched (:name fd)] (get result (tuple w))))
                 sts ws)))
     sts
     ;; one call per fetch per plan: the same fetch name in two declarations is two fetches
     (group-by (fn [{:keys [i fd]}] [(get-in sts [i :plan]) (:name fd)])
               (remove :roots wanted)))))

(defn- remember! [{:keys [draft plan mask]}]
  (if (zero? mask)
    (.remove passed draft)
    (.put passed draft [plan mask])))

(defn- run-all
  "Run the checks of every draft, and of every draft under it, and return one vector of nodes per draft."
  [drafts names]
  (let [nodes        (pure-phase (into [] (comp (map-indexed (fn [i d] (flatten-tree i d names))) cat) drafts))
        unproven     (into #{}
                           (comp (filter (fn [{:keys [failed needed plan]}]
                                           (or (seq failed) (not-every? (comp needed :name) (:pure plan)))))
                                 (map :tree))
                           nodes)
        ;; nothing in a tree touches the world until every pure check in it has run and passed
        nodes        (mapv #(assoc % :world? (not (unproven (:tree %)))) nodes)
        nodes        (fetch-phase nodes)
        nodes        (mapv #(if (or (:world? %) (seq (:failed %))) (run-external %) %) nodes)
        by-tree      (group-by :tree nodes)]
    (run! remember! nodes)
    (mapv #(get by-tree % []) (range (count drafts)))))

(defn- tree-failures
  "Every failure in the tree, shaped like the tree: the root's checks at the top, and each child's under its field
  (and index, for `children`). `nil` when there are none."
  [nodes]
  (not-empty
   (reduce (fn [m {:keys [path failed]}]
             (cond
               (empty? failed) m
               (empty? path)   (merge m failed)
               :else           (update-in m path merge failed)))
           {}
           nodes)))

(defn- tree-status
  "The status of the first pure failure in the tree, children first, or else of the first external one."
  [nodes]
  (let [first-of (fn [pure?]
                   (some (fn [{:keys [plan order]}]
                           (some #(let [c ((:checks-by-name plan) %)] (when (= pure? (boolean (:pure? c))) c)) order))
                         nodes))
        c        (or (first-of true) (first-of false))]
    (or (:status-code c) (if (:pure? c) 400 403))))

(defn- construct ^Object [^Class cls fields values]
  (Reflector/invokeConstructor cls (object-array (map #(get values %) fields))))

(defn- issue-tree
  "The parsed root of the tree: every node's value, built in the pure phase, remembered as issued."
  [nodes]
  (doseq [{:keys [view plan]} nodes]
    (.put issued view plan))
  (:view (peek nodes)))

(defn- invalid! [nodes data]
  (throw (ex-info (str "Invalid " (.getSimpleName ^Class (:parses-to (:plan (peek nodes)))))
                  (merge {:status-code (tree-status nodes)} data))))

;;; ------------------------------------------------ API ------------------------------------------------

(defn check-all
  "Run the checks named in `names` (nil for all) against each draft, with what they wait on, and every pure check of
  every draft under it. A tree fetches and runs external checks only when every pure check in it ran and passed, so
  with a subset of `names` the children's external checks may not run. Returns a vector aligned with `drafts`: `true`,
  or the failures, shaped like the tree. Each fetch is called once per declaration for all the drafts. A pure check
  that passed is remembered for that draft."
  [drafts names]
  (mapv #(or (tree-failures %) true) (run-all drafts names)))

(defn check
  "[[check-all]] for one draft."
  [draft names]
  (first (check-all [draft] names)))

(defn passed-checks
  "The names of the pure checks `draft` is known to pass."
  [draft]
  (let [{:keys [pure], :as plan} (plan-for draft)
        mask                     (remembered-mask draft plan)]
    (into #{} (comp (filter #(bit-test mask (:bit %))) (map :name)) pure)))

(defn assoc-draft
  "`draft` with `field` set to `v`, keeping any pass remembered for the checks that do not read `field`. This is what
  `assoc` on a draft does; call `assoc`."
  [draft field v]
  (let [{:keys [fields field-mask draft-class], :as plan} (plan-for draft)]
    (when-not (some #{field} fields)
      (no-field! draft field))
    (let [result (construct draft-class fields (assoc (read-fields draft fields) field v))
          mask   (bit-and-not (remembered-mask draft plan) (long (get field-mask field 0)))]
      ;; a declaration may vary with a field: the result's plan may not be the one these bits were set under
      (when (and (not (zero? mask)) (identical? plan (plan-for result)))
        (.put passed result [plan mask]))
      result)))

(defn parse-all
  "Parse each draft and the drafts under it. Returns a vector of parsed values aligned with `drafts`, or throws once
  with every failure, keyed by index. The status is that of the first draft that failed. Exceptions out of a check or
  fetch propagate, so an external check that throws stops the call."
  [drafts]
  (let [trees    (run-all drafts nil)
        failures (into (sorted-map) (keep-indexed (fn [i nodes] (some->> (tree-failures nodes) (vector i)))) trees)]
    (when (seq failures)
      (invalid! (nth trees (key (first failures))) {:failures failures}))
    (mapv issue-tree trees)))

(defn parse
  "Parse `draft` and the drafts under it, skipping pure checks already known to pass. Returns a new value of its parsed
  type, whose child fields hold parsed values, or throws with the failures shaped like the tree. The status is the
  first pure failure's `:status-code` (400 by default), else the first external one's (403). Exceptions out of a check
  or fetch propagate."
  [draft]
  (let [nodes (first (run-all [draft] nil))]
    (when-let [failures (tree-failures nodes)]
      (invalid! nodes {:draft (class draft), :failures failures}))
    (issue-tree nodes)))

(defn parsed?
  "Whether `x` is a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`."
  [parsed-type x]
  (and (some? x)
       (member? parsed-type (class x))
       (.containsKey issued x)))

(defn parsed
  "`x`, which must be a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`. Throws
  a 500 otherwise. Hint the result with the type to read its fields with `.-`."
  [parsed-type x]
  (when-not (parsed? parsed-type x)
    ;; A programming error, not a permission failure: the checks ran or never did.
    (throw (ex-info (str "Expected a parsed " (type-name parsed-type))
                    {:status-code 500
                     :expected    parsed-type
                     :got         (type x)})))
  x)

(defn draft-of
  "A draft with the fields of `parsed`, a value [[parse]] issued, known to pass every pure check. Child fields hold
  drafts of the children, known to pass theirs."
  [parsed]
  (let [plan (.get issued parsed)]
    (when-not plan
      (throw (ex-info "Expected a value parse issued" {:status-code 500, :got (type parsed)})))
    (let [values (reduce-kv (fn [vs field {:keys [kind]}]
                              (update vs field #(if (= :one kind) (some-> % draft-of) (mapv draft-of %))))
                            (into {} (map (fn [f] [f (get parsed f)])) (:fields plan))
                            (:children plan))
          draft  (construct (:draft-class plan) (:fields plan) values)]
      (when-not (zero? (long (:all-pure-mask plan)))
        (.put passed draft [plan (:all-pure-mask plan)]))
      draft)))

;;; ------------------------------------------------ Building and reading ------------------------------------------------

(defn draft-from-map
  "A draft of `draft-type` with the fields in `m`, nil for each one `m` leaves out. A key that is not a field is a 400:
  `m` is usually a request. Called by [[draft]]."
  [^Class draft-type m]
  (when-not (map? m)
    (throw (ex-info (str "Expected a map to build a " (.getSimpleName draft-type)) {:status-code 400, :got (type m)})))
  (let [fields (basis draft-type)]
    (when-let [unknown (seq (remove (set fields) (keys m)))]
      (throw (ex-info (str (.getSimpleName draft-type) " has no fields " (vec unknown))
                      {:status-code 400, :unknown-fields (vec unknown), :fields fields})))
    (construct draft-type fields m)))

(defmacro draft
  "A draft of the parsed type `parsed-type`, defined with [[defparsed]], with the fields in `m`.

  When `m` is a literal map, it must name every field, and only fields: anything else is a compile error, so write
  `nil` for a field you mean to leave empty. A map built at run time, usually a request, may leave fields out, which
  are nil; a key that is not a field is a 400.

    (pt/draft Transfer {:user-id u, :from-id 1, :to-id 2, :amount 30M})"
  [parsed-type m]
  (let [cls   (resolve parsed-type)
        dcls  (when (class? cls) (.get drafts-by-parsed-type cls))
        _     (when-not dcls
                (throw (ex-info (str "draft: " parsed-type " is not a type defined with defparsed") {})))
        dsym  (symbol (.getName ^Class dcls))
        flds  (basis dcls)]
    (if (map? m)
      (do (when-let [unknown (seq (remove (set flds) (keys m)))]
            (throw (ex-info (str "draft: " parsed-type " has no fields " (vec unknown) "; its fields are " flds) {})))
          (when-let [missing (seq (remove (set (keys m)) flds))]
            (throw (ex-info (str "draft: " parsed-type " needs every field; missing " (vec missing)
                                 ". Write nil for a field you mean to leave empty.") {})))
          ;; bind in the order written, so the values are evaluated as they read
          (let [syms (into {} (map (fn [k] [k (gensym (name k))])) (keys m))]
            `(let [~@(mapcat (fn [[k v]] [(syms k) v]) m)]
               (new ~dsym ~@(map syms flds)))))
      `(draft-from-map ~dsym ~m))))

(defn- let-fields
  "The fields every value of `type-sym` has, when it names a parsed type or a sum, at compile time."
  [type-sym]
  (let [x (resolve type-sym)
        v (if (var? x) @x x)]
    (cond
      (parsed-type? v) (set (basis v))
      (sum? v)         (apply set/intersection (map (comp set basis) (:members v)))
      :else            (throw (ex-info (str "pt/let: " type-sym " is not a parsed type or a sum") {})))))

(defn- check-let-binding!
  "Refuse, at compile time, a destructuring of `(pt/parsed Type x)` that reads a key `Type` does not have."
  [form init]
  (let [type-sym (second init)
        fields   (let-fields type-sym)
        refuse!  (fn [msg] (throw (ex-info (str "pt/let: " msg ": " (pr-str form)) {:form form})))
        field!   (fn [k] (when-not (contains? fields k)
                           (refuse! (str type-sym " has no field " k "; its fields are " (vec (sort fields))))))]
    (cond
      (symbol? form) nil
      (vector? form) (refuse! "a parsed value is a map; destructure it with {:keys [...]}")
      (map? form)    (doseq [[k b] form]
                       (cond
                         (= :keys k) (doseq [s b]
                                       (when-not (simple-symbol? s)
                                         (refuse! ":keys takes unqualified symbols"))
                                       (field! (keyword s)))
                         (= :as k)   nil
                         (= :or k)   (refuse! ":or never applies: a parsed value has every field")
                         (keyword? k) (refuse! (str k " is not allowed; use :keys or {local :field}"))
                         :else       (do (when-not (simple-keyword? b)
                                           (refuse! "a {local :field} pair needs an unqualified keyword"))
                                         (field! b))))
      :else          (refuse! "unexpected binding form"))))
