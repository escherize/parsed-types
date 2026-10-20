(ns parsed-types.core
  "Parse, don't validate, for Clojure records.

  A draft is a `defrecord` that declares its checks. [[parse]] runs them and returns a new value of a separate parsed
  type, or throws. A function that needs checked data takes the parsed type and nothing else:

    (pt/defdraft TransferDraft -> Transfer [user-id from-id to-id amount]
      (pure ::amount-positive \"The amount is a positive decimal.\"
            [amount]
            (and (decimal? amount) (pos? amount)))
      (external ::funds-available \"The source account holds the amount.\"
                [amount] [source ::source]
                (<= amount (:balance source)))
      (fetch ::source [from-id] accounts-by-id))

    (defn transfer! [transfer]
      (let [^Transfer t (pt/parsed Transfer transfer)]
        (move-money! (.-from-id t) (.-to-id t) (.-amount t))))

  A check receives exactly the fields in its argument vector, then the fetches it binds. It passes when it returns
  exactly `true`; anything else is the failure, reported under its name.

  A `pure` check reads only its arguments, and its pass is remembered for the draft. An `external` one reads the
  world, and runs on every call. Nothing fetches and no external check runs until every pure check in the tree
  passes.

  A field declared with `children` or `child` holds drafts, and one [[parse]] parses the whole tree.

  A parsed value is a read-only map. `assoc`, `dissoc` and `with-meta` throw: to change one, take [[draft-of]] it,
  [[assoc-field]], and [[parse]] again."
  (:require
   [clojure.set :as set]
   [potemkin.collections :as p.collections])
  (:import
   (clojure.lang IRecord Reflector)
   (com.google.common.collect MapMaker)
   (java.lang.reflect Field Modifier)
   (java.util Map)
   (java.util.concurrent ConcurrentMap)))

(set! *warn-on-reflection* true)

(defprotocol Draft
  "A record that [[parse]] turns into a parsed type."
  (declaration [draft]
    "`{:parses-to ParsedType, :checks [check ...], :fetches [fetch ...], :children {field child}}`. Return the same
    `def`ed map every time. [[defdraft]] writes it for you.

    `:parses-to` is a class [[defparsed]] defined. Its fields must be the draft's fields, in the same order, and no
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

(defn final!
  "Throws: a parsed value cannot be changed. Called by the types [[defparsed]] defines."
  [parsed op]
  (throw (ex-info (str (.getSimpleName (class parsed)) " is a parsed value and cannot be changed with " op
                       ". Use draft-of, then assoc-field, then parse.")
                  {:status-code 500, :parsed (class parsed), :op op})))

(defmacro defparsed
  "Define `name`, a parsed type with `fields`: a read-only map that only [[parse]] issues. `assoc`, `dissoc` and
  `with-meta` throw. Read fields with `.-` on a hinted local, or as keys."
  [name fields]
  (let [k (gensym "k")]
    `(do
       (p.collections/def-map-type ~name [~@fields]
         parsed_types.core.ParsedValue
         (~'get [_# ~k default#]
          (case ~k
            ~@(mapcat (fn [f] [(keyword f) f]) fields)
            default#))
         (~'assoc [this# _# _#] (parsed-types.core/final! this# "assoc"))
         (~'dissoc [this# _#] (parsed-types.core/final! this# "dissoc"))
         (~'keys [_#] ~(mapv keyword fields))
         (~'meta [_#] nil)
         (~'with-meta [this# _#] (parsed-types.core/final! this# "with-meta")))
       ;; `#ns.Type{...}`, like a record, so a parsed value is told apart from a plain map at the REPL. The reader
       ;; cannot build one back: a type is not a record.
       (defmethod print-method ~name [x# ^java.io.Writer w#]
         (.write w# (str "#" (.getName (class x#))))
         (print-method (into {} x#) w#))
       ~name)))

;;; ------------------------------------------------ defdraft ------------------------------------------------

(defn- syntax-error! [form message]
  (throw (ex-info (str "defdraft: " message ": " (pr-str form)) {:form form})))

(defn- parse-check-clause
  "`(pure name doc opts? [field ...] body ...)` or `(external name doc opts? [field ...] [local fetch ...]? body ...)`"
  [fields fetch-names [head n doc & more :as form]]
  (let [external?      (= 'external head)
        [opts more]    (if (map? (first more)) [(first more) (rest more)] [{} more])
        [args & more]  more
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
      (syntax-error! form "a check's fetches are pairs of a local and a fetch declared in this defdraft"))
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

(defmacro defdraft
  "Define `draft-name`, a draft record, and `parsed-name`, the type [[parse]] turns it into, both with `fields`, and
  the checks that stand between them.

    (defdraft PayrollDraft -> Payroll [user-id from-id transfers]
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
  error. Defines `<draft-name>-declaration`, private, holding the declaration."
  [draft-name arrow parsed-name fields & clauses]
  (when-not (= '-> arrow) (syntax-error! &form "write (defdraft DraftName -> ParsedName [fields] clauses ...)"))
  (when-not (and (vector? fields) (every? simple-symbol? fields)) (syntax-error! &form "fields must be a vector of symbols"))
  (when-not (apply distinct? ::sentinel fields) (syntax-error! &form "a field is named twice"))
  (doseq [c clauses]
    (when-not (and (seq? c) ('#{pure external fetch child children} (first c)))
      (syntax-error! c "a clause is (pure ...), (external ...), (fetch ...), (child ...) or (children ...)")))
  (let [by-head     (group-by first clauses)
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
        decl-sym    (symbol (str draft-name "-declaration"))]
    (when-not (apply distinct? ::sentinel names) (syntax-error! &form "a check or fetch name is used twice"))
    `(do
       (defparsed ~parsed-name ~fields)
       (def ~(vary-meta decl-sym assoc :private true)
         {:parses-to ~parsed-name
          :checks    ~checks
          :fetches   ~fetches
          :children  ~children})
       (defrecord ~draft-name ~fields
         Draft
         (~'declaration [~'_] ~decl-sym))
       ~draft-name)))

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

;; Declaration -> its plan, by identity.
(defonce ^:private ^Map plans
  (.makeMap (.weakKeys (MapMaker.))))

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

(defn- draft-record? [x]
  (and (instance? IRecord x) (satisfies? Draft x)))

(defn- parsed-type? [x]
  (and (class? x) (.isAssignableFrom ParsedValue ^Class x)))

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
    (doseq [[field {:keys [kind type]}] children]
      (when-not (contains? fields field)
        (bad-declaration! draft-class "a child field is not a field of the draft" {:field field}))
      (when-not (and (#{:one :many} kind) (parsed-type? type))
        (bad-declaration! draft-class "a child is {:kind :one|:many, :type ParsedType}" {:field field})))
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
  (let [drafts-it? (fn [d] (and (draft-record? d) (identical? parsed-type (:parses-to (plan-for d)))))]
    (case kind
      :one  (or (nil? v) (drafts-it? v))
      :many (and (vector? v) (every? drafts-it? v)))))

(defn- child-shape-check
  "The pure check every child field gets: it holds drafts of the declared type. Every check that reads the field waits
  on it."
  [field {:keys [kind type]}]
  {:name  (keyword "parsed-types.child" (name field))
   :doc   (str (name field) " holds " (if (= :one kind) "nil or a draft" "a vector of drafts") " that parse to "
               (.getSimpleName ^Class type) ".")
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
  (when-not (draft-record? draft)
    (throw (ex-info (str "Not a draft record: " (type draft)) {:status-code 500, :got (type draft)})))
  (let [decl (declaration draft)
        plan (or (.get plans decl)
                 (let [plan (compile-plan (class draft) decl)]
                   (.put plans decl plan)
                   plan))]
    (when-not (identical? (class draft) (:draft-class plan))
      (bad-declaration! (class draft) "shares its declaration with another draft type"
                        {:other (:draft-class plan)}))
    plan))

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
  "The deepest tree [[parse]] takes. A deeper one is refused with a 400 rather than a stack overflow later."
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
  (loop [stack (list [root [] names 0]), pre []]
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
    (let [remembered? (and (:pure? c) (bit-test (:mask st) (:bit c)))
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

(defn- pure-phase
  "Run every node's pure checks, children before parents. A parent's checks see each child as the map of values its
  own checks saw, and a node is bad when it, or anything under it, failed."
  [nodes]
  (first (reduce (fn [[out bad views] {:keys [tree path child-paths plan values], :as st}]
                   (let [inputs (reduce-kv (fn [m field paths]
                                             (assoc m field (if (= :one (get-in plan [:children field :kind]))
                                                              (views [tree (first paths)])
                                                              (mapv #(views [tree %]) paths))))
                                           values child-paths)
                         st     (run-pure (assoc st :inputs inputs) bad)
                         bad?   (or (seq (:failed st)) (some #(bad [tree %]) (apply concat (vals child-paths))))]
                     [(conj out st) (cond-> bad bad? (conj [tree path])) (assoc views [tree path] inputs)]))
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
  "The parsed root of the tree, its child fields holding the parsed children, every value remembered as issued."
  [nodes]
  (get (reduce (fn [by-path {:keys [plan values path child-paths]}]
                 (let [values (reduce-kv (fn [vs field paths]
                                           (assoc vs field (if (= :one (get-in plan [:children field :kind]))
                                                             (by-path (first paths))
                                                             (mapv by-path paths))))
                                         values child-paths)
                       parsed (construct (:parses-to plan) (:fields plan) values)]
                   (.put issued parsed plan)
                   (assoc by-path path parsed)))
               {}
               nodes)
       []))

(defn- invalid! [nodes data]
  (throw (ex-info (str "Invalid " (.getSimpleName ^Class (:parses-to (:plan (peek nodes)))))
                  (merge {:status-code (tree-status nodes)} data))))

;;; ------------------------------------------------ API ------------------------------------------------

(defn check-all
  "Run the checks named in `names` (nil for all) against each draft, with what they wait on, and every check of every
  draft under it. Returns a vector aligned with `drafts`: `true`, or the failures, shaped like the tree. Each fetch is
  called once per declaration for all the drafts. A pure check that passed is remembered for that draft."
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

(defn assoc-field
  "`draft` with `field` set to `v`. The checks that do not read `field` keep any pass remembered for `draft`."
  [draft field v]
  (let [{:keys [fields field-mask draft-class], :as plan} (plan-for draft)]
    (when-not (some #{field} fields)
      (throw (ex-info (str (.getSimpleName ^Class draft-class) " has no field " field)
                      {:status-code 500, :draft draft-class, :field field})))
    (let [result (assoc draft field v)
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
  "Whether `x` is a value of `parsed-type` that [[parse]] issued."
  [^Class parsed-type x]
  (and (instance? parsed-type x)
       (.containsKey issued x)))

(defn parsed
  "`x`, which must be a value of `parsed-type` that [[parse]] issued. Throws a 500 otherwise. Hint the result with the
  type to read its fields with `.-`."
  [^Class parsed-type x]
  (when-not (parsed? parsed-type x)
    ;; A programming error, not a permission failure: the checks ran or never did.
    (throw (ex-info (str "Expected a parsed " (.getSimpleName parsed-type))
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
