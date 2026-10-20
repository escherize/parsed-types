(ns parsed-types.core
  "Parse, don't validate, for Clojure.

  [[defparsed]] defines a parsed type, its draft, and the checks between them. [[parse]] runs the checks on a draft and
  returns a new value of the parsed type, or throws. A function that needs checked data takes the parsed type and
  nothing else:

    (pt/defparsed Debit [user-id account-id amount]
      (pure ::amount-positive \"The amount is a positive decimal.\"
            [amount]
            (and (decimal? amount) (pos? amount)))
      (external ::covers-amount \"The account holds the amount.\"
                [amount] [account ::account]
                (<= amount (:balance account 0M)))
      (fetch ::account [account-id] accounts-by-id))

    (defn debit! [debit]
      (let [{:keys [account-id amount]} (pt/parsed Debit debit)]
        (add-to-balance! account-id (- amount))))

  A check receives exactly the fields in its argument vector, then the fetches it binds. It passes when it returns
  exactly `true`; anything else is the failure, reported under its name.

  A `pure` check reads only its arguments. An `external` one reads the world. Nothing fetches and no external check
  runs until every pure check in the tree passes.

  A field declared with `children` or `child` holds drafts, and one [[parse]] parses the whole tree.

  Drafts and parsed values are maps with fixed fields: reading a key that is not a field throws. A parsed value cannot
  change: to change one, take [[draft-of]] it, `assoc`, and [[parse]] again.

  Every error the library throws is an `ex-info` whose ex-data has a `:type` in this namespace: `::invalid` (checks
  failed), `::not-parsed`, `::cannot-change`, `::no-field`, `::bad-input`, `::bad-declaration`, `::bad-fetch` or
  `::stale-type`. Its message says what went wrong and how to fix it."
  (:require
   [clojure.set :as set]
   [clojure.string])
  (:import
   (clojure.lang Reflector)
   (com.google.common.collect MapMaker)
   (java.lang.reflect Field Modifier)
   (java.util Map)))

(set! *warn-on-reflection* true)

(definterface ParsedValue)

(definterface DraftValue)

(defn final!
  "Throws `::cannot-change`: a parsed value cannot be changed. Called by the parsed types [[defparsed]] defines."
  [parsed op]
  (let [n (.getSimpleName (class parsed))]
    (throw (ex-info (if (= "with-meta" op)
                      (str n " is a parsed value and cannot carry metadata. Keep the metadata beside it, not on it.")
                      (str n " is a parsed value and cannot be changed with " op ". Use (pt/draft-of x), assoc, then"
                           " pt/parse."))
                    {:type ::cannot-change, :parsed (.getName (class parsed)), :op op}))))

(defn fixed!
  "Throws `::cannot-change`: a draft's fields are fixed. Called by the draft types [[defparsed]] defines."
  [draft op]
  (let [n (.getSimpleName (class draft))]
    (throw (ex-info (case op
                      "dissoc"    (str n " has fixed fields and cannot be changed with dissoc. Use assoc to set the"
                                       " field to nil.")
                      "with-meta" (str n " is a draft and cannot carry metadata. Keep the metadata beside it, not on"
                                       " it.")
                      (str n " has fixed fields and cannot be changed with " op ". Use assoc."))
                    {:type ::cannot-change, :draft (.getName (class draft)), :op op}))))

(defn no-field!
  "Throws `::no-field`: `x` has no field `k`. Called by the types [[defparsed]] defines, when a key that is not a
  field is read without a default, or set on a draft."
  [x k]
  (let [fields (vec (keys x))]
    (throw (ex-info (str (.getSimpleName (class x)) " has no field " (pr-str k) ". Use one of its fields " fields
                         ", or give get a default.")
                    {:type                                        ::no-field
                     (if (instance? DraftValue x) :draft :parsed) (.getName (class x))
                     :key                                         k
                     :fields                                      fields}))))

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
      ;; like APersistentMap.cons: nil, a map entry, a pair, or a seq of entries
      (~'cons [~this ~o]
       (clojure.core/cond
         (clojure.core/nil? ~o)                   ~this
         (clojure.core/instance? java.util.Map$Entry ~o)
         (clojure.core/assoc ~this (.getKey ~(with-meta o {:tag 'java.util.Map$Entry}))
                             (.getValue ~(with-meta o {:tag 'java.util.Map$Entry})))
         (clojure.core/vector? ~o)
         (if (clojure.core/= 2 (clojure.core/count ~o))
           (clojure.core/assoc ~this (clojure.core/nth ~o 0) (clojure.core/nth ~o 1))
           (throw (IllegalArgumentException. "Vector arg to map conj must be a pair")))
         :else
         (clojure.core/reduce (fn [m# e#] (clojure.core/assoc m# (.getKey ^java.util.Map$Entry e#)
                                                             (.getValue ^java.util.Map$Entry e#)))
                              ~this
                              (clojure.core/seq ~o))))
      (~'empty [~this] {})
      ;; like defrecord: the same class, and each field `=`, so 1.0M equals 1.00M and an Integer equals a Long
      (~'equiv [~this ~o] (clojure.core/and (clojure.core/identical? (clojure.core/class ~this) (clojure.core/class ~o))
                                             ~@(map (fn [fld] `(clojure.core/= ~fld (.valAt ~(with-meta o {:tag 'clojure.lang.ILookup})
                                                                                                ~(keyword fld))))
                                                    fields)))
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
  (let [refuse! (fn [message] (throw (ex-info message {:type ::bad-declaration, :form form})))]
    (when-not (and (vector? fields) (every? simple-symbol? fields))
      (refuse! (str "fields must be a vector of simple symbols. Write them like [user-id amount]: " (pr-str form))))
    (when-not (apply distinct? ::sentinel fields)
      (refuse! (str "a field is named twice. Name each field once: " (pr-str form))))
    (when-let [special (seq (filter #(or (special-symbol? %) (= '& %)) fields))]
      (refuse! (str "a field cannot be named like a special form: " (vec special) ". Rename it.")))
    (when-let [dotted (seq (filter #(clojure.string/includes? (name %) ".") fields))]
      (refuse! (str "a field name cannot contain a dot: " (vec dotted) ". Use a dash instead.")))
    (when-let [clash (seq (for [[_ fs] (group-by #(munge (name %)) fields) :when (< 1 (count fs))] fs))]
      (refuse! (str "these fields compile to the same Java name: " (vec (first clash)) ". Rename one of them.")))
    (when-let [reserved (seq (filter '#{__meta __extmap __hash __hasheq} fields))]
      (refuse! (str "deftype reserves these field names: " (vec reserved) ". Rename them.")))))

(defn- print-method-form [name]
  ;; `#ns.Type{...}`, like a record, so a value is told apart from a plain map at the REPL. The reader cannot build one
  ;; back: a map type is not a record.
  `(defmethod print-method ~name [x# ^java.io.Writer w#]
     (.write w# (str "#" (.getName (class x#))))
     (print-method (into {} x#) w#)))

(defn- defparsed-type
  "The form that defines `name`, a bare parsed type with `fields`: a map that only [[parse]] issues. `assoc`, `dissoc`
  and `with-meta` throw, and so does reading a key that is not a field. Only [[defparsed]] calls this, when it
  expands: it is private, so a parsed type cannot be defined apart from its draft and its checks."
  [name fields]
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v")]
    `(do
       (deftype ~name [~@fields]
         parsed_types.core.ParsedValue
         ~@(map-type-methods fields this k v `(final! ~this "assoc") `final!))
       ~(print-method-form name))))

(defn- defdraft-type
  "The form that defines `name`, a bare draft type with `fields`. `assoc` sets a field; `dissoc` and `with-meta` throw,
  and so do reading or setting a key that is not a field. Only [[defparsed]] calls this, when it expands."
  [name fields]
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v")]
    `(do
       (deftype ~name [~@fields]
         parsed_types.core.DraftValue
         ~@(map-type-methods fields this k v `(assoc-draft ~this ~k ~v) `fixed!))
       ~(print-method-form name))))

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
    (throw (ex-info (str "defsum " name ": members must be distinct types defined with defparsed. List each parsed"
                         " type once, such as (defsum Transaction [Debit Credit]).")
                    {:type ::bad-declaration, :name name, :members (mapv pr-str members)})))
  (->Sum name (vec members)))

(defmacro defsum
  "Define `name`, a closed set of parsed types. [[match]] on it must handle every member, and a child field of a sum
  type holds drafts that parse to any member.

    (defsum Transaction [Debit Credit])"
  [name members]
  `(def ~name (sum '~name ~members)))

(defmacro match
  "Branch on which member of the sum `sum-name` the parsed value `x` is. Each clause is a member type, a binding, and
  an expression:

    (match Transaction transaction
      Debit  debit  (debit! debit)
      Credit credit (credit! credit))

  Leaving out a member, naming a type that is not one, or naming one twice is a compile error. Each binding is hinted
  with its type, and holds `x` only after [[parsed]] accepts it. Any other `x` throws `::not-parsed`."
  [sum-name x & clauses]
  (let [refuse! (fn [message data] (throw (ex-info message (assoc data :type ::bad-declaration))))
        s       (let [v (resolve sum-name)] (when (var? v) @v))
        _       (when-not (sum? s)
                  (refuse! (str "match: " sum-name " is not a sum defined with defsum. Name a sum, or define one with"
                                " defsum.")
                           {}))
        _       (when-not (zero? (mod (count clauses) 3))
                  (refuse! (str "match: each clause is a type, a binding, and an expression. Write clauses like"
                                " Debit d (f d).")
                           {:clauses clauses}))
        triples (partition 3 clauses)
        types   (map (fn [[t]] (let [c (resolve t)]
                                 (when-not (class? c) (refuse! (str "match: " t " is not a type. Name a member of "
                                                                    sum-name ".")
                                                               {}))
                                 c))
                     triples)
        members (set (:members s))
        missing (remove (set types) (:members s))
        extra   (remove members types)]
    (when (seq missing)
      (refuse! (str "match on " sum-name " does not handle " (mapv #(.getSimpleName ^Class %) missing)
                    ". Add a clause for each.")
               {}))
    (when (seq extra)
      (refuse! (str "match on " sum-name ": not members: " (mapv #(.getSimpleName ^Class %) extra)
                    ". Remove those clauses, or add the types to the defsum.")
               {}))
    (when-not (apply distinct? types)
      (refuse! (str "match on " sum-name " handles a type twice. Keep one clause per type.") {}))
    (let [v (gensym "v")]
      `(let [~v ~x]
         (cond
           ~@(mapcat (fn [[t b e]]
                       [`(instance? ~t ~v) `(let [~(vary-meta b assoc :tag t) (parsed ~t ~v)] ~e)])
                     triples)
           ;; `v` is no member, so this throws
           :else (parsed ~sum-name ~v))))))

;;; ------------------------------------------------ defparsed ------------------------------------------------

(defn- syntax-error! [form message]
  (throw (ex-info (str "defparsed: " message " In: " (pr-str form)) {:type ::bad-declaration, :form form})))

(defn- parse-check-clause
  "`(pure name doc [field ...] body ...)` or `(external name doc [field ...] [local fetch ...]? body ...)`"
  [fields fetch-names [head n doc & more :as form]]
  (let [external?      (= 'external head)
        _              (when (map? (first more))
                         (syntax-error! form (str "checks take no options. Remove " (pr-str (first more)) ".")))
        [args & more]  more
        _              (when (and external? (vector? (first more)) (nil? (next more))
                                  (seq (first more)) (every? keyword? (take-nth 2 (rest (first more)))))
                         (syntax-error! form "a check needs a body after its fetch bindings. Write the body next."))
        [binds body]   (if (and external? (vector? (first more)) (next more))
                         [(first more) (rest more)]
                         [[] more])
        pairs          (partition-all 2 binds)]
    (when-not (qualified-keyword? n)
      (syntax-error! form "a check's name must be a qualified keyword. Write it like ::amount-positive."))
    (when-not (string? doc)
      (syntax-error! form "a check needs a doc string after its name. Say what the check checks, in a sentence."))
    (when (and (not external?) (vector? (first more)) (next more))
      (syntax-error! form "a pure check cannot fetch. Make it external, or remove the fetch bindings."))
    (when-not (and (vector? args) (every? symbol? args))
      (syntax-error! form (str "a check needs a vector of the fields it reads. Write it after the doc string, like"
                               " [amount].")))
    (when-let [unknown (seq (remove (set fields) args))]
      (syntax-error! form (str "a check reads fields the draft does not have: " (vec unknown) ". Use only its fields "
                               fields ".")))
    (when-not (apply distinct? ::sentinel args)
      (syntax-error! form "a check reads a field twice. Name each field once."))
    (when-not (every? #(and (= 2 (count %)) (symbol? (first %)) (contains? fetch-names (second %))) pairs)
      (syntax-error! form (str "a check's fetches are pairs of a local and a fetch declared in this defparsed. Write"
                               " them like [account ::account], naming one of " (vec (sort fetch-names)) ".")))
    (when-not (apply distinct? ::sentinel (map first pairs))
      (syntax-error! form "a check binds two fetches to one local. Give each fetch its own local."))
    (when-let [clash (seq (filter (set fields) (map first pairs)))]
      (syntax-error! form (str "a fetch is bound to a field's name: " (vec clash) ". Pick another local.")))
    (when (empty? body) (syntax-error! form "a check needs a body. Write an expression that returns true to pass."))
    ;; An unnamed field is usually an unresolved symbol, but a field named like a clojure.core fn (`name`, `count`)
    ;; would resolve to that fn instead, so look for every field by name.
    (when-let [unnamed (seq (filter (set (remove (set args) fields)) (filter symbol? (tree-seq coll? seq body))))]
      (syntax-error! form (str "a check uses fields its argument vector does not name: " (vec (distinct unnamed))
                               ". Add them to the argument vector.")))
    (merge {:name    n
            :doc     doc
            :kind    (if external? :external :pure)
            :reads   (mapv keyword args)
            :f       `(fn [~@args ~@(map first pairs)] ~@body)}
           (when (seq pairs) {:fetches (mapv second pairs)}))))

(defn- parse-fetch-clause [fields [_ n reads f :as form]]
  (when-not (qualified-keyword? n)
    (syntax-error! form "a fetch's name must be a qualified keyword. Write it like ::account."))
  (when-not (and (vector? reads) (every? (set fields) reads))
    (syntax-error! form (str "a fetch needs a vector of fields of the draft. Name some of " fields ".")))
  (when-not f (syntax-error! form "a fetch needs a function of the tuples it fetches. Write it after the fields."))
  {:name n, :reads (mapv keyword reads), :f f})

(defmacro defparsed
  "Define `parsed-name`, a parsed type with `fields`, its draft type `<parsed-name>Draft` with the same fields, and the
  checks [[parse]] runs to turn a draft into a parsed value. Build a draft with `->NameDraft` or [[draft]].

    (defparsed JournalEntry [user-id lines]
      (children lines Transaction)
      (pure ::one-user \"Every line is made by the entry's user.\"
            [user-id lines]
            (every? #(= user-id (:user-id %)) lines))
      (pure ::balanced \"The debits and the credits add up to the same amount.\"
            [lines]
            (= (reduce + 0M (map :amount (filter #(instance? Debit %) lines)))
               (reduce + 0M (map :amount (filter #(instance? Credit %) lines))))))

  Clauses:

  - `(pure name doc [field ...] body ...)`: a check over the named fields.
  - `(external name doc [field ...] [local fetch-name ...]? body ...)`: a check over the named fields and the
    results of the named fetches.
  - `(fetch name [field ...] f)`: `f` takes every distinct tuple of the fields, and returns a map from each tuple to
    what it fetched.
  - `(children field ParsedType)`: the field holds a vector of drafts that parse to `ParsedType`.
  - `(child field ParsedType)`: the field holds nil or a draft that parses to `ParsedType`.

  A check takes no options. Its argument vector is the fields it reads: one that is not a field is a compile error.

  The plan [[parse]] follows is built and checked when the form loads, so every declaration error throws
  `::bad-declaration` then, not at the first parse. Only `defparsed` defines parsed types: the draft type and the
  parsed type are defined together, so the draft type is the only one that parses to its parsed type."
  [parsed-name fields & clauses]
  (when-not (simple-symbol? parsed-name)
    (syntax-error! &form "write (defparsed Name [fields] clauses ...), with a symbol for Name."))
  (check-field-names! &form fields)
  (doseq [c clauses]
    (when-not (and (seq? c) (symbol? (first c)) ('#{pure external fetch child children} (symbol (name (first c)))))
      (syntax-error! c (str "a clause is (pure ...), (external ...), (fetch ...), (child ...) or (children ...). Use"
                            " one of those."))))
  (let [clauses     (map #(cons (symbol (name (first %))) (rest %)) clauses)
        by-head     (group-by first clauses)
        fetches     (mapv #(parse-fetch-clause fields %) (get by-head 'fetch))
        fetch-names (set (map :name fetches))
        checks      (mapv #(parse-check-clause fields fetch-names %)
                          (filter (comp '#{pure external} first) clauses))
        children    (into {}
                          (for [[head field type :as c] (concat (get by-head 'child) (get by-head 'children))]
                            (do (when-not (contains? (set fields) field)
                                  (syntax-error! c (str "a child field must be a field of the draft. Name one of "
                                                        fields ".")))
                                [(keyword field) {:kind (if (= 'child head) :one :many), :type type}])))
        child-heads (map second (concat (get by-head 'child) (get by-head 'children)))
        _           (when-not (apply distinct? ::sentinel child-heads)
                      (syntax-error! &form (str "a field is declared as a child twice. Declare it once, with child or"
                                                " children.")))
        names       (concat (map :name checks) (map :name fetches))
        draft-name  (symbol (str parsed-name "Draft"))]
    (when-not (apply distinct? ::sentinel names)
      (syntax-error! &form "a check or fetch name is used twice. Give each its own name."))
    (when-let [reserved (seq (filter #(= "parsed-types.child" (namespace %)) names))]
      (syntax-error! &form (str "names in parsed-types.child are reserved for child-field checks: " (vec reserved)
                                ". Name them in your own namespace.")))
    `(do
       ~(defparsed-type parsed-name fields)
       ~(defdraft-type draft-name fields)
       ;; through `var`, because `register!` is private: only this expansion may record a plan
       ((var register!) ~parsed-name ~draft-name {:checks ~checks, :fetches ~fetches, :children ~children})
       ~parsed-name)))

;;; ------------------------------------------------ State ------------------------------------------------

;; Every value [[parse]] issued -> the plan of the draft it came from, by identity: Guava's weak keys compare with
;; `identical?`, not `=`, so an equal value built with the constructor is not in here. A secret in a field or in
;; metadata would not do: either can be read out of one parsed value and put into a forged one. Weak, so a parsed value
;; is garbage collected like any other.
(defonce ^:private ^Map issued
  (.makeMap (.weakKeys (MapMaker.))))

;; Draft type -> its plan, and parsed type -> the same plan, by identity. Written once per class, when its `defparsed`
;; loads. Weak, so the classes a REPL replaces can be collected.
(defonce ^:private ^Map registry
  (.makeMap (.weakKeys (MapMaker.))))

;;; ------------------------------------------------ Plans ------------------------------------------------

(declare plan-for)

(defn- bad-declaration! [draft-class message data]
  (throw (ex-info (str (.getSimpleName ^Class draft-class) ": " message)
                  (merge {:type ::bad-declaration, :draft (.getName ^Class draft-class)} data))))

(defn- closed-over-locals
  "The locals `f` closes over. A check must close over none, or what it reads would not be what it declares."
  [f]
  (into []
        (comp (remove #(Modifier/isStatic (.getModifiers ^Field %)))
              (map #(.getName ^Field %)))
        (.getDeclaredFields (class f))))

(defn- basis [^Class cls]
  (mapv keyword (Reflector/invokeStaticMethod cls "getBasis" (object-array 0))))

(defn- draft-value?
  "Whether `x` is a draft of a type [[defparsed]] defined. A type that only implements `DraftValue` is not."
  [x]
  (and (instance? DraftValue x) (some? (.get registry (class x)))))

(defn- validate-declaration!
  "Throw `::bad-declaration` for what `defparsed` cannot see when it expands, only when it loads: a child type that is
  not a parsed type or a sum, a fetch that is not a fn, and a pure check that closes over a local."
  [draft-class {:keys [checks fetches children]}]
  (doseq [[field {:keys [type]}] children]
    (when-not (or (parsed-type? type) (sum? type))
      (bad-declaration! draft-class (str "the child field " field " holds " (pr-str type) ", which is not a type defined"
                                         " with defparsed or a sum defined with defsum. Name one of those.")
                        {:field field})))
  (doseq [{n :name, :keys [f]} fetches]
    (when-not (fn? f)
      (bad-declaration! draft-class (str "the fetch " n " is " (pr-str f) ", not a fn. Write a fn of the tuples it"
                                         " fetches.")
                        {:name n})))
  (doseq [{n :name, :keys [kind f]} checks]
    (when-let [locals (and (= :pure kind) (seq (closed-over-locals f)))]
      (bad-declaration! draft-class (str "the pure check " n " closes over locals " (mapv symbol locals) ". A pure check"
                                         " reads only the fields it names. Read the value from a field, or make the"
                                         " check external.")
                        {:name n, :locals locals}))))

(defn- stale-type!
  "Throws `::stale-type`: `parent` declared `field` against `old`, a class a REPL has since replaced with another of
  the same name."
  [^Class parent field type ^Class old]
  (let [p (.getSimpleName parent), c (.getSimpleName old)]
    (throw (ex-info (if (sum? type)
                      (str p " was defined against an older " c ", through the sum " (:name type) ". Re-evaluate the"
                           " defsum for " (:name type) ", then the defparsed for " p ".")
                      (str p " was defined against an older " c ". Re-evaluate the defparsed for " p "."))
                    {:type ::stale-type, :parent (.getName parent), :field field, :child (.getName old)}))))

(defn- children-of-type?
  "Whether `v` is what the child field `field` of `parent`, of `kind` and `parsed-type`, may hold. Throws
  `::stale-type` for a draft that parses to a class with the name of a declared member, but not that class: the member
  was re-evaluated at a REPL after `parent` was defined. Members are never matched by name, so one tree never mixes
  old and new classes."
  [parent field kind parsed-type v]
  (let [drafts-it? (fn [d]
                     (and (draft-value? d)
                          (let [^Class cls (:parses-to (plan-for d))
                                old        (some #(when (= (.getName ^Class %) (.getName cls)) %)
                                                 (if (sum? parsed-type) (:members parsed-type) [parsed-type]))]
                            (cond
                              (member? parsed-type cls) true
                              old                       (stale-type! parent field parsed-type old)
                              :else                     false))))]
    (case kind
      :one  (or (nil? v) (drafts-it? v))
      :many (and (vector? v) (every? drafts-it? v)))))

(defn- child-shape-check
  "The pure check every child field of `parent` gets: it holds drafts of the declared type. Every check that reads the
  field waits on it."
  [parent field {:keys [kind type]}]
  {:name  (keyword "parsed-types.child" (name field))
   :doc   (str (name field) " holds " (if (= :one kind) "nil or a draft that parses to " "a vector of drafts that parse to ")
               (type-name type) ".")
   :kind  :pure
   :shape? true
   :reads [field]
   :f     (fn [v] (children-of-type? parent field kind type v))})

(defn- reads-set [x] (set (:reads x)))

(defn- compile-plan
  "Order the checks, and work out what each waits on.

  - A pure check waits on every pure check that reads a proper subset of its fields: a check on one field (its shape)
    runs before a check relating it to another. A check that reads a child field also waits on that field's shape
    check, and on the children's own pure checks.
  - A fetch waits on every pure check, so malformed input never reads the world.
  - An external check waits on its fetches and on every pure check.

  Pure checks run first, fewest fields first, then fetches, then external checks, each in declaration order."
  [parses-to draft-class {:keys [checks fetches children], :as decl}]
  (validate-declaration! draft-class decl)
  (let [shape     (into {} (map (fn [[field child]] [field (child-shape-check parses-to field child)])) children)
        pure      (into (vec (vals shape)) (filter #(= :pure (:kind %))) checks)
        external  (filterv #(= :external (:kind %)) checks)
        waits-on  (fn [pred] (into #{} (comp (filter pred) (map :name)) pure))
        all-pure  (waits-on any?)
        shapes-of (fn [c] (into #{} (keep #(:name (get shape %))) (:reads c)))
        pure'     (for [c pure
                        :let [rs (reads-set c)]]
                    (assoc c
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
     :declaration     (assoc decl :parses-to parses-to)
     :fields          (basis draft-class)
     :children        children
     :pure            (vec (sort-by (comp count reads-set) pure'))
     :fetches         (vec fetches')
     :external        (vec external')
     :checks-by-name  (into {} (map (juxt :name identity)) (concat pure' external'))
     :fetches-by-name (into {} (map (juxt :name identity)) fetches')}))

(defn- describe
  "`x` for an error message: nil, a plain map, or a value of its type."
  [x]
  (cond
    (nil? x)                                            "nil"
    (and (map? x) (not (instance? ParsedValue x))
         (not (instance? DraftValue x)))                "a plain map"
    :else                                               (str "a " (.getSimpleName (class x)))))

(defn- register!
  "Build and check the plan of `draft-type`, which parses to `parsed-type`, and record it for both. Called by the
  expansion of [[defparsed]], when it loads; throws `::bad-declaration` then."
  [parsed-type draft-type decl]
  (let [plan (compile-plan parsed-type draft-type decl)]
    (.put registry draft-type plan)
    (.put registry parsed-type plan)))

(defn- plan-for [draft]
  (when-not (draft-value? draft)
    (throw (ex-info (str "Expected a draft defined with defparsed, got " (describe draft) "."
                         (if (instance? ParsedValue draft)
                           " It is already parsed: call (pt/draft-of x) to get its draft."
                           " Build one with its ->NameDraft constructor or pt/draft."))
                    {:type ::bad-input, :got (some-> draft class .getName)})))
  (.get registry (class draft)))

(defn declaration
  "The declaration `defparsed` wrote for `draft`'s type: `{:parses-to :checks :fetches :children}`, as data. Throws
  `::bad-input` for anything but a draft."
  [draft]
  (:declaration (plan-for draft)))

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
                    (bad-declaration! (:draft-class plan)
                                      (str "no check or fetch is named " (pr-str n) ". Name one of "
                                           (vec (sort (concat (keys (:checks-by-name plan))
                                                              (keys (:fetches-by-name plan)))))
                                           ", or pass nil to run every check.")
                                      {:check n}))]
          (recur (into (vec more) (:prereqs x)) (conj seen n))))
      seen)))

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
     :fetched {}
     :failed  {}     ; check name -> failure entry
     :order   []     ; failed check names, in the order they failed
     :roots   {}}))  ; blocked check or fetch name -> the failed checks that blocked it

(defn- child-path [path field i]
  (if i (conj path field i) (conj path field)))

(def ^:private max-depth
  "The most levels a tree [[parse]] takes may have, counting the root. A deeper one throws `::bad-input`."
  1000)

(def ^:private max-nodes
  "The most drafts one tree may hold, counting a draft each time it appears."
  100000)

(defn- too-big! [message]
  (throw (ex-info message {:type ::bad-input, :max-depth max-depth, :max-nodes max-nodes})))

(defn- flatten-tree
  "The drafts of the tree under `root`, children before parents, each with its `:path` from the root. `names` applies
  to the root; every child runs all its checks."
  [tree root names]
  (loop [stack (list [root [] names 1]), pre []]
    (if-let [[draft path nm depth] (first stack)]
      (let [_    (when (< max-depth depth)
                   (too-big! (str "The tree is deeper than " max-depth " levels, counting the root. Parse a shallower"
                                  " tree.")))
            _    (when (<= max-nodes (count pre))
                   (too-big! (str "The tree holds more than " max-nodes " drafts, counting a draft each time it"
                                  " appears. Parse it in smaller parts.")))
            st   (assoc (start draft nm) :tree tree :path path)
            kids (for [[field {:keys [kind type]}] (get-in st [:plan :children])
                       :let  [v (get-in st [:values field])]
                       :when (children-of-type? (get-in st [:plan :parses-to]) field kind type v)
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
    (let [result (apply f (concat (map #(get (if (:shape? c) (:values st) (:inputs st)) %) reads)
                                  (map #(get-in st [:fetched %]) fetches)))]
      (if (true? result)
        st
        (-> st
            (assoc-in [:failed n] {:doc doc, :returned result})
            (update :order conj n))))))

(defn- failed-children
  "The paths of the children in the fields `c` reads that failed, or have a failure under them, in this phase. A shape
  check reads only the field's form, so it waits on no child."
  [st c bad]
  (when-not (:shape? c)
    (seq (for [field (:reads c)
               path  (get-in st [:child-paths field])
               :when (bad [(:tree st) path])]
           path))))

(defn- run-checks
  "Run `checks` in order. A check that reads a child field whose children failed is held, not run: it would read
  values that are not proven."
  [st checks bad]
  (reduce (fn [st {n :name, :keys [doc prereqs], :as c}]
            (if-let [paths (and (not (roots-of st prereqs)) (failed-children st c bad))]
              (-> st
                  (assoc-in [:failed n] {:doc doc, :blocked-by (vec paths)})
                  (update :order conj n))
              (run-check st c)))
          st
          (filter (comp (:needed st) :name) checks)))

(defn- run-pure [st bad]
  (run-checks st (get-in st [:plan :pure]) bad))

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

(defn- external-phase
  "Run every node's external checks, children before parents, holding a parent's checks over children that failed
  theirs. A node in a tree that failed a pure check runs them only to record what its own failures blocked."
  [nodes]
  (first (reduce (fn [[out bad] {:keys [tree path child-paths world? failed], :as st}]
                   (let [st   (if (or world? (seq failed)) (run-checks st (get-in st [:plan :external]) bad) st)
                         bad? (or (seq (:failed st)) (some #(bad [tree %]) (apply concat (vals child-paths))))]
                     [(conj out st) (cond-> bad bad? (conj [tree path]))]))
                 [[] #{}]
                 nodes)))

(defn- fetch-phase
  "Run every fetch the nodes need, each once per plan across all of them, with each distinct tuple of arguments once."
  [sts]
  ;; only nodes of trees whose every pure check passed reach the world, so no fetch here waits on a failure
  (let [wanted (for [[i st] (map-indexed vector sts)
                     :when  (:world? st)
                     fd     (get-in st [:plan :fetches])
                     :when  ((:needed st) (:name fd))]
                 {:i i, :fd fd})
        tuple  (fn [{:keys [i fd]}] (mapv #(get (:inputs (nth sts i)) %) (:reads fd)))]
    (reduce
     (fn [sts [_ ws]]
       (let [{fd :fd} (first ws)
             tuples   (into [] (comp (map tuple) (distinct)) ws)
             result   ((:f fd) tuples)]
         (when-let [missing (seq (remove #(and (map? result) (contains? result %)) tuples))]
           (throw (ex-info (if (map? result)
                             (str "Fetch " (:name fd) " returned no entry for " (vec missing) ". Return a key for"
                                  " every tuple, with nil for not found.")
                             (str "Fetch " (:name fd) " returned " (describe result) ", not a map. Return a map with a"
                                  " key for every tuple, with nil for not found."))
                           {:type ::bad-fetch, :fetch (:name fd), :missing (vec missing)})))
         (reduce (fn [sts w]
                   (assoc-in sts [(:i w) :fetched (:name fd)] (get result (tuple w))))
                 sts ws)))
     sts
     ;; one call per fetch per plan: the same fetch name in two declarations is two fetches
     (group-by (fn [{:keys [i fd]}] [(get-in sts [i :plan]) (:name fd)]) wanted))))

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
        nodes        (external-phase nodes)
        by-tree      (group-by :tree nodes)]
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

(defn- failed-kinds
  "The kinds, `:pure` or `:external`, of the checks that returned a failure in `nodes`. A held check returned nothing:
  what holds it failed under it."
  [nodes]
  (into #{}
        (for [{:keys [plan failed]} nodes
              [n entry]             failed
              :when                 (contains? entry :returned)]
          (:kind ((:checks-by-name plan) n)))))

(defn- failed-names
  "Each check that returned a failure in `nodes`, in the order they ran, with its path when it is under the root."
  [nodes]
  (for [{:keys [path failed order]} nodes
        n                           order
        :when                       (contains? (failed n) :returned)]
    (str n (when (seq path) (str " at " path)))))

(defn- construct ^Object [^Class cls fields values]
  (Reflector/invokeConstructor cls (object-array (map #(get values %) fields))))

(defn- issue-tree
  "The parsed root of the tree: every node's value, built in the pure phase, remembered as issued."
  [nodes]
  (doseq [{:keys [view plan]} nodes]
    (.put issued view plan))
  (:view (peek nodes)))

(defn- invalid! [what names nodes data]
  (throw (ex-info (str "Invalid " what ": " (clojure.string/join ", " names) " failed. Change the draft so these checks"
                       " pass and parse again; (:failures (ex-data e)) has each one's doc and what it returned.")
                  (merge {:type ::invalid, :failed-kinds (failed-kinds nodes)} data))))

;;; ------------------------------------------------ API ------------------------------------------------

(defn check-all
  "Run the checks named in `names` (nil for all) against each draft, with what they wait on, and every pure check of
  every draft under it. A tree fetches and runs external checks only when every pure check in it ran and passed, so
  with a subset of `names` the children's external checks may not run. Returns a vector aligned with `drafts`: `true`,
  or the failures, shaped like the tree. Each fetch is called once per declaration for all the drafts. Issues
  nothing: a pass here is not proof, so [[parse]] runs every check again."
  [drafts names]
  (mapv #(or (tree-failures %) true) (run-all drafts names)))

(defn check
  "[[check-all]] for one draft."
  [draft names]
  (first (check-all [draft] names)))

(defn assoc-draft
  "`draft` with `field` set to `v`: a new draft. This is what `assoc` on a draft does; call `assoc`."
  [draft field v]
  (let [{:keys [fields draft-class]} (plan-for draft)]
    (when-not (some #{field} fields)
      (no-field! draft field))
    (construct draft-class fields (assoc (read-fields draft fields) field v))))

(defn parse-all
  "Parse each draft and the drafts under it. Returns a vector of parsed values aligned with `drafts`, or throws
  `::invalid` once with every failure: `:failures` maps the index of each draft that failed to its failures, shaped
  like [[parse]]'s, and `:failed-kinds` holds the kinds of check that failed in any of them. Exceptions out of a check
  or fetch propagate, so an external check that throws stops the call."
  [drafts]
  (let [trees    (run-all drafts nil)
        failures (into (sorted-map) (keep-indexed (fn [i nodes] (some->> (tree-failures nodes) (vector i)))) trees)]
    (when (seq failures)
      (let [failing (map #(nth trees %) (keys failures))]
        (invalid! (str "drafts at indexes " (vec (keys failures)))
                  (for [i (keys failures), n (failed-names (nth trees i))] (str n " in draft " i))
                  (apply concat failing)
                  {:failures failures})))
    (mapv issue-tree trees)))

(defn parse
  "Parse `draft` and the drafts under it, running every check. Returns a new value of its parsed
  type, whose child fields hold parsed values. When a check fails, throws `::invalid` with:

  - `:failures`: every failure, shaped like the tree: the root's checks at the top, and each child's under its field
    (and index, for `children`).
  - `:failed-kinds`: a set of `:pure` and `:external`, the kinds of check that failed anywhere in the tree. A pure
    failure means the input is malformed; an external one means a check of the world refused it.
  - `:draft`: the name of the draft's class.

  Exceptions out of a check or fetch propagate."
  [draft]
  (let [nodes (first (run-all [draft] nil))]
    (when-let [failures (tree-failures nodes)]
      (invalid! (.getSimpleName ^Class (:parses-to (:plan (peek nodes)))) (failed-names nodes) nodes
                {:draft (.getName (class draft)), :failures failures}))
    (issue-tree nodes)))

(defn parsed?
  "Whether `x` is a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`."
  [parsed-type x]
  (and (some? x)
       (member? parsed-type (class x))
       (.containsKey issued x)))

(defn- build-hint
  "How to get a parsed value of `type`, for an error message."
  [type]
  (cond
    (sum? type)
    (str "Build a draft of a member (" (clojure.string/join " or " (map type-name (:members type)))
         ") and call pt/parse.")

    (.get registry type)
    (str "Build a " (.getSimpleName ^Class (:draft-class (.get registry type))) " and call pt/parse.")

    :else
    (str "Build a draft that parses to " (type-name type) " and call pt/parse.")))

(defn parsed
  "`x`, which must be a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`. Throws
  `::not-parsed` otherwise. Read its fields by keyword, as in `(:amount x)`: a key that is not a field throws
  `::no-field`."
  [parsed-type x]
  (when-not (parsed? parsed-type x)
    (throw (ex-info (str "Expected a parsed " (type-name parsed-type) ", got " (describe x)
                         (cond
                           (and (draft-value? x) (member? parsed-type (:parses-to (plan-for x))))
                           ". Call pt/parse on it first."

                           (member? parsed-type (class x))
                           (str " that parse did not issue. " (build-hint parsed-type))

                           :else
                           (str ". " (build-hint parsed-type))))
                    {:type     ::not-parsed
                     :expected (if (sum? parsed-type) (str (:name parsed-type)) (.getName ^Class parsed-type))
                     :got      (some-> x class .getName)})))
  x)

(defn draft-of
  "A draft with the fields of `parsed`, a value [[parse]] issued. Child fields hold drafts of the children. Throws `::not-parsed` for any other value."
  [parsed]
  (let [plan (.get issued parsed)]
    (when-not plan
      (throw (ex-info (if (instance? DraftValue parsed)
                        (str "pt/draft-of takes a value pt/parse issued, got " (describe parsed) ", which is already a"
                             " draft. Use it as it is.")
                        (str "pt/draft-of takes a value pt/parse issued, got " (describe parsed)
                             (when (instance? ParsedValue parsed) " that parse did not issue")
                             ". Build a draft with its ->NameDraft constructor or pt/draft instead."))
                      {:type ::not-parsed, :got (some-> parsed class .getName)})))
    (let [values (reduce-kv (fn [vs field {:keys [kind]}]
                              (update vs field #(if (= :one kind) (some-> % draft-of) (mapv draft-of %))))
                            (into {} (map (fn [f] [f (get parsed f)])) (:fields plan))
                            (:children plan))]
      (construct (:draft-class plan) (:fields plan) values))))

;;; ------------------------------------------------ Building and reading ------------------------------------------------

(defn draft-from-map
  "A draft of `draft-type` with the fields in `m`, nil for each one `m` leaves out. A non-map, or a key that is not a
  field, throws `::bad-input`. Called by [[draft]]."
  [^Class draft-type m]
  (let [fields (basis draft-type)]
    (when-not (map? m)
      (throw (ex-info (str "Expected a map to build a " (.getSimpleName draft-type) ", got " (describe m)
                           ". Pass a map with some of its fields " fields ".")
                      {:type ::bad-input, :got (some-> m class .getName)})))
    (when-let [unknown (seq (remove (set fields) (keys m)))]
      (throw (ex-info (str (.getSimpleName draft-type) " has no fields " (vec unknown) ". Remove them, or use its"
                           " fields " fields ".")
                      {:type ::bad-input, :unknown-fields (vec unknown), :fields fields})))
    (construct draft-type fields m)))

(defmacro draft
  "A draft of the parsed type `parsed-type`, defined with [[defparsed]], with the fields in `m`.

  When `m` is a literal map, it must name every field, and only fields: anything else is a compile error, so write
  `nil` for a field you mean to leave empty. A map built at run time, usually a request, may leave fields out, which
  are nil; a key that is not a field throws `::bad-input`.

    (pt/draft Debit {:user-id u, :account-id 1, :amount 30M})"
  [parsed-type m]
  (let [refuse! (fn [message] (throw (ex-info message {:type ::bad-input})))
        cls     (resolve parsed-type)
        dcls    (when (and (parsed-type? cls) (.get registry cls)) (:draft-class (.get registry cls)))
        _       (when-not dcls
                  (refuse! (str "draft: " parsed-type " is not a type defined with defparsed. Name a parsed type that"
                                " defparsed defined, not a sum or a draft.")))
        dsym    (symbol (.getName ^Class dcls))
        flds    (basis dcls)]
    (if (map? m)
      (do (when-let [unknown (seq (remove (set flds) (keys m)))]
            (refuse! (str "draft: " parsed-type " has no fields " (vec unknown) "; its fields are " flds
                          ". Remove the others.")))
          (when-let [missing (seq (remove (set (keys m)) flds))]
            (refuse! (str "draft: " parsed-type " needs every field; missing " (vec missing)
                          ". Write nil for a field you mean to leave empty.")))
          ;; bind in the order written, so the values are evaluated as they read
          (let [syms (into {} (map (fn [k] [k (gensym (name k))])) (keys m))]
            `(let [~@(mapcat (fn [[k v]] [(syms k) v]) m)]
               (new ~dsym ~@(map syms flds)))))
      `(draft-from-map ~dsym ~m))))
