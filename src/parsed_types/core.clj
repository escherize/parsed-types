(ns parsed-types.core
  "Parse, don't validate, for Clojure records.

  A draft is a `defrecord` that declares its checks. [[parse]] runs them and returns a new value of a parsed type,
  declared with [[defparsed]], or throws. A sink takes the parsed type and nothing else:

    (defparsed TableRename [user-id table-id name])

    (def ^:private table-rename-declaration
      {:parses-to TableRename
       :checks    [{:name  ::name-not-blank
                    :doc   \"The name is a non-blank string.\"
                    :kind  :pure
                    :reads [:name]
                    :f     (fn [name] (and (string? name) (not (str/blank? name))))}
                   {:name  ::may-write-table
                    :doc   \"The current user may write the table.\"
                    :kind  :external
                    :reads [:table-id]
                    :f     (fn [table-id] (may-write? table-id))}]})

    (defrecord TableRenameDraft [user-id table-id name]
      pt/Draft
      (declaration [_] table-rename-declaration))

    (defn rename-table! [rename]
      (let [^TableRename rename (pt/parsed TableRename rename)]
        (write! (.-table-id rename) (.-name rename))))

  A check receives exactly what it names: the fields under `:reads`, then the fetches under `:fetches`, in that
  order. It passes when it returns exactly `true`. Anything else is the failure, reported under its name.

  A `:pure` check reads only its arguments, and its pass is remembered for the draft. An `:external` one reads the
  world, and runs on every call. Only an external check may name `:fetches`. Nothing fetches and no external check
  runs until every pure check passes.

  A parsed value is a read-only map. `assoc`, `dissoc` and `with-meta` throw: to change one, take [[draft-of]] it,
  [[assoc-field]], and [[parse]] again."
  (:require
   [clojure.set :as set]
   [potemkin.collections :as p.collections])
  (:import
   (clojure.lang Reflector)
   (com.google.common.collect MapMaker)
   (java.lang.reflect Field Modifier)
   (java.util Map)))

(set! *warn-on-reflection* true)

(defprotocol Draft
  "A record that [[parse]] turns into a parsed type."
  (declaration [draft]
    "`{:parses-to ParsedType, :checks [check ...], :fetches [fetch ...]}`. Return the same `def`ed map every time.

    `:parses-to` is the class [[defparsed]] defined. Its fields must be the draft's fields, in the same order.

    A check is `{:name :doc :kind :reads :fetches :status-code :f}`. `:kind` is `:pure`, for a check that reads only
    its arguments, whose pass is remembered, or `:external`, for one that reads the world, which runs on every call.
    Only an external check may name `:fetches`. `:status-code` is the HTTP status when this check is the first to
    fail; the default is 400 for a pure check and 403 for an external one.

    A pure check's pass is remembered for the draft, so the fields it reads must be immutable values, and its `:f`
    must not read a global that can change, such as an atom. The check that an `:f` closes over no locals does not see
    globals. It also refuses `constantly`, `partial`, `comp`, `memoize` and `with-meta` on a fn, which close over
    their arguments: write a `fn`.

    A fetch is `{:name :reads :f}`. Its `:f` takes every distinct tuple of the fields it reads, across all the drafts
    in the call, and returns a map from each tuple to what it fetched."))

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
    `(p.collections/def-map-type ~name [~@fields]
       (~'get [_# ~k default#]
        (case ~k
          ~@(mapcat (fn [f] [(keyword f) f]) fields)
          default#))
       (~'assoc [this# _# _#] (parsed-types.core/final! this# "assoc"))
       (~'dissoc [this# _#] (parsed-types.core/final! this# "dissoc"))
       (~'keys [_#] ~(mapv keyword fields))
       (~'meta [_#] nil)
       (~'with-meta [this# _#] (parsed-types.core/final! this# "with-meta")))))

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

;; Declaration -> its plan, by identity.
(defonce ^:private ^Map plans
  (.makeMap (.weakKeys (MapMaker.))))

;;; ------------------------------------------------ Plans ------------------------------------------------

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

(defn- validate-declaration! [draft-class {:keys [parses-to checks fetches]}]
  (let [fields      (set (basis draft-class))
        fetch-names (into #{} (map :name) fetches)
        names       (concat (map :name checks) (map :name fetches))]
    (when-not (and (class? parses-to) (.isAssignableFrom clojure.lang.IPersistentMap parses-to))
      (bad-declaration! draft-class ":parses-to must be a type defined with defparsed" {:parses-to parses-to}))
    (when-not (= (basis draft-class) (basis parses-to))
      (bad-declaration! draft-class "the parsed type's fields must be the draft's fields, in order"
                        {:draft-fields (basis draft-class), :parsed-fields (basis parses-to)}))
    (when-let [dupes (seq (for [[n c] (frequencies names) :when (< 1 c)] n))]
      (bad-declaration! draft-class "a check or fetch name is used twice" {:names dupes}))
    (doseq [{n :name, :keys [doc kind reads f], fs :fetches} (concat checks fetches)
            :let [check? (contains? (set (map :name checks)) n)]]
      (when-not (qualified-keyword? n)
        (bad-declaration! draft-class "a check or fetch name must be a qualified keyword" {:name n}))
      (when-not (fn? f)
        (bad-declaration! draft-class "a check or fetch needs an :f" {:name n}))
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
      (when-let [locals (seq (closed-over-locals f))]
        (bad-declaration! draft-class (str "an :f closes over locals. It must read only its arguments, so that what"
                                           " it reads is what it declares.")
                          {:name n, :locals locals})))
    (when (< 64 (count (filter #(= :pure (:kind %)) checks)))
      (bad-declaration! draft-class "more than 64 pure checks" {}))))

(defn- reads-set [x] (set (:reads x)))

(defn- compile-plan
  "Order the checks, and work out what each waits on.

  - A pure check waits on every pure check that reads a proper subset of its fields: a check on one field (its shape)
    runs before a check relating it to another.
  - A fetch waits on every pure check, so malformed input never reads the world.
  - An external check waits on its fetches and on every pure check.

  Pure checks run first, fewest fields first, then fetches, then external checks, each in declaration order."
  [draft-class {:keys [parses-to checks fetches], :as decl}]
  (validate-declaration! draft-class decl)
  (let [pure      (filterv #(= :pure (:kind %)) checks)
        external  (filterv #(= :external (:kind %)) checks)
        bit       (zipmap (map :name pure) (range))
        waits-on  (fn [pred] (into #{} (comp (filter pred) (map :name)) pure))
        all-pure  (waits-on any?)
        pure'     (for [c pure
                        :let [rs (reads-set c)]]
                    (assoc c
                           :pure?   true
                           :bit     (bit (:name c))
                           :prereqs (waits-on #(and (not= (:name %) (:name c))
                                                    (set/subset? (reads-set %) rs)
                                                    (not= (reads-set %) rs)))))
        fetches'  (for [fd fetches]
                    (assoc fd :prereqs all-pure))
        external' (for [c external]
                    (assoc c :prereqs (into all-pure (:fetches c))))]
    {:draft-class     draft-class
     :parses-to       parses-to
     :fields          (basis draft-class)
     :pure            (vec (sort-by (comp count :reads) pure'))
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
  (when-not (satisfies? Draft draft)
    (throw (ex-info (str "Not a Draft: " (type draft)) {:status-code 500, :got (type draft)})))
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

(defn- start [draft names]
  (let [plan (plan-for draft)]
    {:draft   draft
     :plan    plan
     :needed  (needed plan (or names (keys (:checks-by-name plan))))
     :mask    (remembered-mask draft plan)
     :fetched {}
     :failed  {}     ; check name -> failure entry
     :order   []     ; failed check names, in the order they failed
     :roots   {}}))  ; blocked check or fetch name -> the failed checks that blocked it

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
                        (apply f (concat (map #(get (:draft st) %) reads)
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

(defn- run-pure [st]
  (reduce run-check st (filter (comp (:needed st) :name) (get-in st [:plan :pure]))))

(defn- run-external [st]
  (reduce run-check st (filter (comp (:needed st) :name) (get-in st [:plan :external]))))

(defn- fetch-phase
  "Run every fetch the drafts need, each once across all of them, with each distinct tuple of arguments once."
  [sts]
  (let [wanted (for [[i st] (map-indexed vector sts)
                     fd     (get-in st [:plan :fetches])
                     :when  ((:needed st) (:name fd))]
                 {:i i, :fd fd, :roots (roots-of st (:prereqs fd))})
        sts    (reduce (fn [sts {:keys [i fd roots]}]
                         (cond-> sts roots (update i block (:name fd) roots false)))
                       sts wanted)
        tuple  (fn [{:keys [i fd]}] (mapv #(get (:draft (nth sts i)) %) (:reads fd)))]
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
     ;; one call per fetch per draft type: the same fetch name on two draft types is two fetches
     (group-by (fn [{:keys [i fd]}] [(get-in sts [i :plan :draft-class]) (:name fd)])
               (remove :roots wanted)))))

(defn- finish
  "Remember the pure checks that passed, and return `true` or the failures."
  [{:keys [draft plan mask failed]}]
  (if (zero? mask)
    (.remove passed draft)
    (.put passed draft [plan mask]))
  (if (empty? failed) true failed))

(defn- run-all [drafts names]
  (as-> (mapv #(start % names) drafts) sts
    (mapv run-pure sts)
    (fetch-phase sts)
    (mapv run-external sts)))

(defn- status-code [{:keys [plan order]}]
  (let [c ((:checks-by-name plan) (first order))]
    (or (:status-code c) (if (:pure? c) 400 403))))

(defn- issue
  "A new value of the draft's parsed type, with the draft's fields, remembered as issued."
  [{:keys [draft plan]}]
  (let [parsed (Reflector/invokeConstructor (:parses-to plan)
                                            (object-array (map #(get draft %) (:fields plan))))]
    (.put issued parsed plan)
    parsed))

;;; ------------------------------------------------ API ------------------------------------------------

(defn check-all
  "Run the checks named in `names` against each draft, with what they wait on, and return a vector aligned with
  `drafts`: `true`, or `{check-name {:doc :returned :blocked}}`. Each fetch is called once for all the drafts. A
  check that did not run because one it waits on failed is named under that check's `:blocked`.

  A pure check that passed is remembered for that draft, and not run again."
  [drafts names]
  (mapv finish (run-all drafts names)))

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
  "Run every check against each draft, with each fetch called once for all of them. Returns a vector of parsed values,
  aligned with `drafts`, or throws once with every failure, by index. The status is that of the first failure of the
  first draft that failed: its check's `:status-code`, or 400 for a pure check and 403 for an external one.
  Exceptions out of a check or fetch propagate, so an external check that throws stops the call at the first draft it
  refuses."
  [drafts]
  (let [sts      (run-all drafts nil)
        results  (mapv finish sts)
        failures (into (sorted-map) (keep-indexed (fn [i r] (when-not (true? r) [i r]))) results)]
    (when (seq failures)
      (let [st (nth sts (key (first failures)))]
        (throw (ex-info (str "Invalid " (.getSimpleName ^Class (get-in st [:plan :parses-to])))
                        {:status-code (status-code st)
                         :failures    failures}))))
    (mapv issue sts)))

(defn parse
  "Run every check of `draft`, skipping pure checks already known to pass. Returns a new value of its parsed type, or
  throws: the first failure's `:status-code`, or 400 when a pure check failed and 403 when an external one did.
  Exceptions out of a check or fetch propagate."
  [draft]
  (let [st     (first (run-all [draft] nil))
        result (finish st)]
    (when-not (true? result)
      (throw (ex-info (str "Invalid " (.getSimpleName ^Class (get-in st [:plan :parses-to])))
                      {:status-code (status-code st)
                       :draft       (class draft)
                       :failures    result})))
    (issue st)))

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
  "A draft with the fields of `parsed`, a value [[parse]] issued, known to pass every pure check."
  [parsed]
  (let [plan (.get issued parsed)]
    (when-not plan
      (throw (ex-info "Expected a value parse issued" {:status-code 500, :got (type parsed)})))
    (let [draft (Reflector/invokeConstructor (:draft-class plan)
                                             (object-array (map #(get parsed %) (:fields plan))))]
      (when-not (zero? (long (:all-pure-mask plan)))
        (.put passed draft [plan (:all-pure-mask plan)]))
      draft)))
