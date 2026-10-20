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

  A parsed value carries its proof: a hidden field that only [[parse]] sets. So [[parsed]] needs no registry of
  issued values, and a value built by hand, equal or not, is refused.

  Parsed types compare by class name, so when a REPL re-evaluates a `defparsed`, code compiled against the old class
  accepts values of the new one, the way a `#'var` follows its latest definition.

  Every error the library throws is an `ex-info` whose ex-data has a `:type` in this namespace: `::invalid` (checks
  failed), `::not-parsed`, `::cannot-change`, `::no-field`, `::bad-input`, `::bad-declaration` or `::bad-fetch`, and
  a `:rule` in `parsed-types.rule` that names the rule it broke. Its message says what happened, with the values, the
  rule, and the fix."
  (:require
   [clojure.set :as set]
   [parsed-types.rule :as-alias rule]
   [clojure.string])
  (:import
   (com.google.common.collect MapMaker)
   (java.lang.reflect Field Modifier)
   (java.util Map)))

(set! *warn-on-reflection* true)

(definterface ParsedValue
  ;; Whether the value's proof is `t`. The proof goes in and only a boolean comes out, so nothing hands the proof out.
  (issuedWith [t]))

;; The proof that [[parse]] issued a value: a parsed value is parsed exactly when its hidden `parse-proof` field holds
;; this object. Only `parse` builds a value with it. `defonce`, so reloading this namespace keeps the values already
;; issued parsed.
(defonce ^:private parse-token (Object.))

(definterface DraftValue)

(defn- edit-distance
  "The Levenshtein distance between the strings `a` and `b`."
  [a b]
  (peek (reduce (fn [prev [i ca]]
                  (reduce (fn [row [j cb]]
                            (conj row (min (inc (peek row))
                                           (inc (nth prev (inc j)))
                                           (+ (nth prev j) (if (= ca cb) 0 1)))))
                          [(inc i)]
                          (map-indexed vector b)))
                (vec (range (inc (count b))))
                (map-indexed vector a))))

(defn- nearest
  "The first of `candidates`, keywords or symbols, within a few edits of `k`, or nil: up to 2, and fewer for a short
  name, so that a one-letter key suggests nothing."
  [k candidates]
  (when (or (keyword? k) (symbol? k))
    (let [n      (name k)
          within (min 2 (quot (count n) 2))
          close  (filter #(<= (edit-distance n (name %)) within) candidates)]
      (first (sort-by #(edit-distance n (name %)) close)))))

(defn- did-you-mean
  "` Did you mean :x?` for the nearest of `candidates` to `k`, or an empty string."
  [k candidates]
  (if-let [m (nearest k candidates)] (str " Did you mean " (pr-str m) "?") ""))

(defn final!
  "Throws `::cannot-change`: a parsed value cannot be changed. `k` is the key `op` was given, or nil. Called by the
  parsed types [[defparsed]] defines."
  [parsed op k]
  (let [n                      (.getSimpleName (class parsed))
        {:keys [rule message]} (case op
                                 "with-meta"
                                 {:rule    ::rule/parsed-no-metadata
                                  :message (str "with-meta on a " n ": a parsed value carries no metadata, so it never"
                                                " changes. Keep the metadata beside it, as in {:value x, :meta m}.")}
                                 "empty"
                                 {:rule    ::rule/parsed-no-empty
                                  :message (str n " has fixed fields, so it has no empty version. Use (into {} x) for"
                                                " a plain map.")}
                                 {:rule    ::rule/parsed-value-is-final
                                  :message (str op " " (pr-str k) " on a " n ": a parsed value never changes, so its"
                                                " checks stay true. Use (pt/parse (assoc (pt/draft-of x) " (pr-str k)
                                                (if (= "dissoc" op) " nil" " v") ")) for a changed copy.")})]
    (throw (ex-info message {:type ::cannot-change, :rule rule, :parsed (.getName (class parsed)), :op op, :key k}))))

(defn fixed!
  "Throws `::cannot-change`: a draft's fields are fixed. `k` is the key `op` was given, or nil. Called by the draft
  types [[defparsed]] defines."
  [draft op k]
  (let [n                      (.getSimpleName (class draft))
        {:keys [rule message]} (case op
                                 "with-meta"
                                 {:rule    ::rule/draft-no-metadata
                                  :message (str "with-meta on a " n ": a draft carries no metadata, so it holds only"
                                                " what parse checks. Keep the metadata beside it, as in {:value x,"
                                                " :meta m}.")}
                                 "empty"
                                 {:rule    ::rule/draft-no-empty
                                  :message (str n " has fixed fields, so it has no empty version. Use (into {} x) for"
                                                " a plain map.")}
                                 {:rule    ::rule/draft-fields-are-fixed
                                  :message (str op " " (pr-str k) " on a " n ": a draft has fixed fields, set with"
                                                " assoc. Use (assoc d " (pr-str k) (if (= "dissoc" op) " nil" " v")
                                                ").")})]
    (throw (ex-info message {:type ::cannot-change, :rule rule, :draft (.getName (class draft)), :op op, :key k}))))

(defn no-field!
  "Throws `::no-field`: `x` has no field `k`. `op` is `\"read\"` when a key that is not a field is read without a
  default, or `\"assoc\"` when one is set on a draft. Called by the types [[defparsed]] defines."
  [x k op]
  (let [fields  (vec (keys x))
        n       (.getSimpleName (class x))
        nearest (nearest k fields)]
    (throw (ex-info (if (= "read" op)
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A key that is not a field"
                           " throws, never reads nil. Read one of its fields " fields ", or read with a default, as"
                           " in (get x " (pr-str k) " default).")
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A draft has fixed fields. Set"
                           " one of " fields ", as in (assoc d " (pr-str (or nearest (first fields))) " v)."))
                    (cond-> {:type                                        ::no-field
                             :rule                                        (if (= "read" op)
                                                                            ::rule/no-such-field-read
                                                                            ::rule/no-such-field-assoc)
                             (if (instance? DraftValue x) :draft :parsed) (.getName (class x))
                             :key                                         k
                             :fields                                      fields}
                      nearest (assoc :suggestion nearest))))))

(defn- map-type-methods
  "The interfaces and methods of a read-only map with exactly `fields`. Reading another key throws unless a default
  is given. `assoc-form`, over `this`, `k` and `v`, is what `assoc` does; `final-name` throws for any other change.
  `same-proof`, of the other value's symbol, is the form that says whether it holds this value's proof, or nil for a
  type without one. Every function in these bodies is written qualified, so no field name can shadow it."
  [fields this k v assoc-form final-name same-proof]
  (clojure.core/let [nf      (gensym "not-found")
                     f       (gensym "f")
                     init    (gensym "init")
                     o       (gensym "o")
                     kws     (mapv keyword fields)
                     lookup  (fn [missing] `(case ~k ~@(mapcat (fn [fld] [(keyword fld) fld]) fields) ~missing))
                     entries `[~@(map (fn [fld] `(clojure.lang.MapEntry/create ~(keyword fld) ~fld)) fields)]]
    ;; not MapEquivalence: like a record, a value is equal only to a value of its own type, never to a plain map. A
    ;; parsed value also holds the same proof: one parse issued never equals one built without it. The hash reads only
    ;; the fields, which equal values share.
    `[clojure.lang.IPersistentMap
      (~'assoc [~this ~k ~v] ~assoc-form)
      (~'assocEx [~this ~k ~v] (~final-name ~this "assocEx" ~k))
      (~'without [~this ~k] (~final-name ~this "dissoc" ~k))
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
      ;; no empty version, so code that rebuilds a map through `empty`, such as clojure.walk, throws instead of
      ;; dropping the type and the proof
      (~'empty [~this] (~final-name ~this "empty" nil))
      ;; like defrecord: the same class, and each field `=`, so 1.0M equals 1.00M and an Integer equals a Long
      (~'equiv [~this ~o] (clojure.core/and (clojure.core/identical? (clojure.core/class ~this) (clojure.core/class ~o))
                                             ~@(when same-proof [(same-proof o)])
                                             ~@(map (fn [fld] `(clojure.core/= ~fld (.valAt ~(with-meta o {:tag 'clojure.lang.ILookup})
                                                                                                ~(keyword fld))))
                                                    fields)))
      (~'seq [~this] (clojure.core/seq ~entries))
      (~'iterator [~this] (clojure.lang.SeqIterator. (clojure.core/seq ~entries)))
      (~'valAt [~this ~k] ~(lookup `(no-field! ~this ~k "read")))
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
      (~'withMeta [~this ~k] (~final-name ~this "with-meta" nil))

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
                                              ~@(when same-proof [(same-proof o)])
                                              (clojure.lang.APersistentMap/mapEquals ~this ~o)))
      (~'toString [~this] (clojure.core/str (clojure.core/into {} ~this)))]))

(defn- syntax-error!
  "Throws `::bad-declaration` for `form`, a `defparsed` form or one of its clauses, that broke `rule`. The message ends
  with the form."
  [form rule message]
  (throw (ex-info (str "defparsed: " message " In: " (pr-str form)) {:type ::bad-declaration, :rule rule, :form form})))

(defn- check-field-names!
  "Refuse fields that cannot be a type's fields: anything but simple symbols, a name twice, or a special form, which
  a field cannot shadow."
  [form fields]
  (when-not (and (vector? fields) (every? simple-symbol? fields))
    (syntax-error! form ::rule/fields-not-symbols
                   (str "the fields " (pr-str fields) " are not a vector of simple symbols. Each field becomes a Java"
                        " field and a keyword key. Write them like [user-id amount].")))
  (when-let [twice (seq (for [[f c] (frequencies fields) :when (< 1 c)] f))]
    (syntax-error! form ::rule/field-named-twice
                   (str "a field is named twice: " (vec twice) ". Each field is one key. Name each field once.")))
  (when-let [special (seq (filter #(or (special-symbol? %) (= '& %)) fields))]
    (syntax-error! form ::rule/field-special-form
                   (str "a field cannot be named like a special form: " (vec special) ". A field is a local in the"
                        " type's methods, and a special form cannot be shadowed. Rename it, as in "
                        (first special) "-value.")))
  (when-let [dotted (seq (filter #(clojure.string/includes? (name %) ".") fields))]
    (syntax-error! form ::rule/field-with-dot
                   (str "a field name cannot contain a dot: " (vec dotted) ". A dot is not allowed in a Java field."
                        " Use a dash instead, as in " (clojure.string/replace (name (first dotted)) "." "-") ".")))
  (when-let [clash (seq (for [[_ fs] (group-by #(munge (name %)) fields) :when (< 1 (count fs))] fs))]
    (syntax-error! form ::rule/fields-same-java-name
                   (str "these fields compile to the same Java name: " (vec (first clash)) ". Each field needs its"
                        " own Java field. Rename one of them.")))
  (when-let [reserved (seq (filter '#{__meta __extmap __hash __hasheq} fields))]
    (syntax-error! form ::rule/field-reserved-by-deftype
                   (str "deftype reserves these field names: " (vec reserved) ". Rename them, as in "
                        (subs (name (first reserved)) 2) ".")))
  (when-let [reserved (seq (filter #(= "parse_proof" (munge (name %))) fields))]
    (syntax-error! form ::rule/field-reserved-for-proof
                   (str "parsed-types reserves the field name parse-proof for the proof a parsed value carries: "
                        (vec reserved) ". Rename it, as in proof."))))

(defn- print-method-form [name]
  ;; `#ns.Type{...}`, like a record, so a value is told apart from a plain map at the REPL. The reader cannot build one
  ;; back: a map type is not a record.
  `(defmethod print-method ~name [x# ^java.io.Writer w#]
     (.write w# (str "#" (.getName (class x#))))
     (print-method (into {} x#) w#)))

(defn- builders
  "The form of the plain fns the plan of `parsed-name` and `draft-name`, with `fields`, uses to build and read values,
  so that parsing calls no reflection:

  - `:make-parsed`, of a map of the fields and a proof, builds a parsed value;
  - `:make-draft`, of a map of the fields, builds a draft;
  - `:read-draft`, of a draft, returns a map of its fields, each read once from its Java field, not through `get`."
  [parsed-name draft-name fields]
  (clojure.core/let [m (gensym "m"), proof (gensym "proof"), d (vary-meta (gensym "d") assoc :tag draft-name)
                     kws (mapv keyword fields)]
    `{:fields      ~kws
      :make-parsed (fn [~m ~proof] (new ~parsed-name ~@(map (fn [k] `(clojure.core/get ~m ~k)) kws) ~proof))
      :make-draft  (fn [~m] (new ~draft-name ~@(map (fn [k] `(clojure.core/get ~m ~k)) kws)))
      :read-draft  (fn [~d] ~(into {} (map (fn [f] [(keyword f) `(. ~d ~(symbol (str "-" f)))])) fields))}))

(defn- defparsed-type
  "The form that defines `name`, a bare parsed type with `fields`: a map that only [[parse]] issues. `assoc`, `dissoc`
  and `with-meta` throw, and so does reading a key that is not a field. Only [[defparsed]] calls this, when it
  expands: it is private, so a parsed type cannot be defined apart from its draft and its checks.

  The type has one more field, `parse-proof`, which is not a key: it is not in the map's keys, seq, count, equality,
  hash or printed form. [[parse]] sets it to the proof; any other constructor call leaves it something else. The field
  is `volatile-mutable`, which Clojure compiles to a field that is not public, so interop cannot read it, and nothing
  sets it after the constructor: code outside the type can only ask `issuedWith` whether it holds a given object.
  The positional factory `->Name` is removed, so code outside this library has no plain function that builds one."
  [name fields]
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v"), t (gensym "t")]
    `(do
       (deftype ~name [~@fields ~(with-meta 'parse-proof {:volatile-mutable true})]
         parsed_types.core.ParsedValue
         (~'issuedWith [~this ~t] (clojure.core/identical? ~t ~'parse-proof))
         ;; the other value is of this class, so it is this generated type, and the proof goes only into it
         ~@(map-type-methods fields this k v `(final! ~this "assoc" ~k) `final!
                             (fn [o]
                               `(.issuedWith ~(with-meta o {:tag 'parsed_types.core.ParsedValue}) ~'parse-proof))))
       (ns-unmap *ns* '~(symbol (str "->" name)))
       ~(print-method-form name))))

(defn- defdraft-type
  "The form that defines `name`, a bare draft type with `fields`. `assoc` sets a field; `dissoc` and `with-meta` throw,
  and so do reading or setting a key that is not a field. Only [[defparsed]] calls this, when it expands."
  [name fields]
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v")]
    `(do
       (deftype ~name [~@fields]
         parsed_types.core.DraftValue
         ~@(map-type-methods fields this k v `(assoc-draft ~this ~k ~v) `fixed! nil))
       ~(print-method-form name))))

;;; ------------------------------------------------ Sums ------------------------------------------------

(defn- parsed-type? [x]
  (and (class? x) (.isAssignableFrom ParsedValue ^Class x)))

(defrecord Sum [name members])

(defn- sum? [x] (instance? Sum x))

(defn- same-type?
  "Whether the classes `a` and `b` are the same parsed type: they have the same name. Two classes have one name only
  when a REPL re-evaluated the `defparsed` that defined them, so the type defined last stands for both, like a var
  named in place of its value."
  [^Class a ^Class b]
  (and (some? a) (some? b) (= (.getName a) (.getName b))))

(defn- member?
  "Whether `cls` is `type`, or one of the members of the sum `type`, by name."
  [type cls]
  (if (sum? type) (boolean (some #(same-type? % cls) (:members type))) (same-type? type cls)))

(defn- type-name [type]
  (if (sum? type) (str (:name type)) (.getSimpleName ^Class type)))

(defn sum
  "A sum of parsed types, named `name`. Use [[defsum]]."
  [name members]
  (when-not (and (seq members) (every? parsed-type? members) (apply distinct? (map #(.getName ^Class %) members)))
    (throw (ex-info (str "defsum " name " got " (mapv #(if (class? %) (symbol (.getSimpleName ^Class %)) %) members)
                         ". A sum's members are distinct types defined with defparsed. List each parsed type once, as"
                         " in (defsum " name " [Debit Credit]).")
                    {:type    ::bad-declaration
                     :rule    ::rule/bad-sum-members
                     :name    name
                     :members (mapv pr-str members)})))
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

  Leaving out a member, naming a type that is not one, or naming one twice is a compile error. A branch runs only when
  [[parsed?]] accepts `x` for its type, compared by name, so a value of a type a REPL re-evaluated still matches. Any
  other `x` throws `::not-parsed`. A binding is not type-hinted: after such a reload the hint would name the old
  class. Read fields by keyword."
  [sum-name x & clauses]
  (let [refuse! (fn [rule message data]
                  (throw (ex-info message (assoc data :type ::bad-declaration, :rule rule, :sum (str sum-name)))))
        s       (let [v (resolve sum-name)] (when (var? v) @v))
        _       (when-not (sum? s)
                  (refuse! ::rule/match-not-a-sum
                           (str "match: " sum-name " is not a sum defined with defsum. match branches on the members"
                                " of a sum. Name a sum, or define one, as in (pt/defsum Transaction [Debit Credit]).")
                           {}))
        _       (when-not (zero? (mod (count clauses) 3))
                  (refuse! ::rule/match-clause-shape
                           (str "match on " sum-name ": " (count clauses) " clause forms, not a multiple of 3. Each"
                                " clause is a type, a binding, and an expression. Write clauses like Debit d (f d).")
                           {:clauses clauses}))
        triples (partition 3 clauses)
        types   (map (fn [[t]] (let [c (resolve t)]
                                 (when-not (class? c)
                                   (refuse! ::rule/match-not-a-type
                                            (str "match on " sum-name ": " t " is not a type. Each clause starts"
                                                 " with a member type. Name one of "
                                                 (mapv #(symbol (.getSimpleName ^Class %)) (:members s)) ".")
                                            {:clause (str t)}))
                                 c))
                     triples)
        name-of (fn [^Class c] (.getName c))
        members (set (map name-of (:members s)))
        missing (remove (set (map name-of types)) (map name-of (:members s)))
        extra   (remove (comp members name-of) types)]
    (when (seq missing)
      (let [simple (mapv #(symbol (peek (clojure.string/split % #"\."))) missing)]
        (refuse! ::rule/match-missing-member
                 (str "match on " sum-name " does not handle " simple ". A match handles every member, so adding"
                      " one to the sum breaks every match that misses it. Add a clause for each, as in "
                      (first simple) " x (f x).")
                 {:missing missing})))
    (when (seq extra)
      (refuse! ::rule/match-not-a-member
               (str "match on " sum-name ": not members: " (mapv #(symbol (.getSimpleName ^Class %)) extra) ". A"
                    " clause handles a member of the sum. Remove those clauses, or add the types to the defsum.")
               {:extra (mapv name-of extra)}))
    (when-not (apply distinct? (map name-of types))
      (refuse! ::rule/match-member-twice
               (str "match on " sum-name " handles a type twice: "
                    (vec (for [[t c] (frequencies (map #(symbol (.getSimpleName ^Class %)) types)) :when (< 1 c)] t))
                    ". One clause runs per member. Keep one clause per type.")
               {}))
    (let [v (gensym "v")]
      `(let [~v ~x]
         (cond
           ~@(mapcat (fn [[t b e]]
                       [`(parsed? ~t ~v) `(let [~b ~v] ~e)])
                     triples)
           ;; `v` is no member, so this throws
           :else (parsed ~sum-name ~v))))))

;;; ------------------------------------------------ defparsed ------------------------------------------------

(defn- parse-check-clause
  "`(pure name doc [field ...] body ...)` or `(external name doc [field ...] [local fetch ...]? body ...)`"
  [fields fetch-names [head n doc & more :as form]]
  (let [external?      (= 'external head)
        what           (str "the check " (pr-str n))
        _              (when (map? (first more))
                         (syntax-error! form ::rule/check-options
                                        (str what " has options " (pr-str (first more)) ", but checks take no options:"
                                             " what a check reads is its argument vector. Remove "
                                             (pr-str (first more)) ".")))
        [args & more]  more
        _              (when (and external? (vector? (first more)) (nil? (next more))
                                  (seq (first more)) (every? keyword? (take-nth 2 (rest (first more)))))
                         (syntax-error! form ::rule/check-no-body-after-fetches
                                        (str what " ends after its fetch bindings " (pr-str (first more)) ". A check"
                                             " needs a body after its fetch bindings, which returns true to pass."
                                             " Write the body next, as in " (pr-str (first more)) " (some? "
                                             (first (first more)) ").")))
        [binds body]   (if (and external? (vector? (first more)) (next more))
                         [(first more) (rest more)]
                         [[] more])
        pairs          (partition-all 2 binds)]
    (when-not (qualified-keyword? n)
      (syntax-error! form ::rule/check-name-not-qualified
                     (str what ": a check's name must be a qualified keyword, so failures from two namespaces never"
                          " collide. Write it like ::" (if (or (keyword? n) (symbol? n)) (name n) "amount-positive")
                          ".")))
    (when-not (string? doc)
      (syntax-error! form ::rule/check-no-doc
                     (str what " needs a doc string after its name: a failure reports it. Write a sentence, as in ("
                          head " " (pr-str n) " \"The amount is positive.\" [amount] ...).")))
    (when (and (not external?) (vector? (first more)) (next more))
      (syntax-error! form ::rule/pure-check-fetches
                     (str what " binds fetches " (pr-str (first more)) ", but a pure check cannot fetch: it reads only"
                          " its fields. Make it (external ...), or remove the fetch bindings.")))
    (when-not (and (vector? args) (every? symbol? args))
      (syntax-error! form ::rule/check-no-field-vector
                     (str what " has " (pr-str args) ", not a vector of the fields it reads, which a check needs."
                          " Write it after the doc string, like [" (first fields) "].")))
    (when-let [unknown (seq (remove (set fields) args))]
      (syntax-error! form ::rule/check-reads-unknown-field
                     (str what " reads fields the draft does not have: " (vec unknown) "."
                          (did-you-mean (first unknown) fields) " Use only its fields " fields ".")))
    (when-let [twice (seq (for [[f c] (frequencies args) :when (< 1 c)] f))]
      (syntax-error! form ::rule/check-reads-field-twice
                     (str what " reads a field twice: " (vec twice) ". Name each field once.")))
    (when-not (every? #(and (= 2 (count %)) (symbol? (first %)) (contains? fetch-names (second %))) pairs)
      (syntax-error! form ::rule/check-bad-fetch-binding
                     (str what " binds " (pr-str binds) ". A check's fetches are pairs of a local and a fetch declared"
                          " in this defparsed. Write them like [account ::account], "
                          (if (seq fetch-names)
                            (str "naming one of " (vec (sort fetch-names)) ".")
                            "and declare the fetch, as in (fetch ::account [account-id] accounts-by-id)."))))
    (when-let [twice (seq (for [[l c] (frequencies (map first pairs)) :when (< 1 c)] l))]
      (syntax-error! form ::rule/check-two-fetches-one-local
                     (str what " binds two fetches to one local: " (vec twice) ". Give each fetch its own local.")))
    (when-let [clash (seq (filter (set fields) (map first pairs)))]
      (syntax-error! form ::rule/fetch-bound-to-field
                     (str what ": a fetch is bound to a field's name: " (vec clash) ". The local would hide the"
                          " field. Pick another local, as in " (first clash) "-row.")))
    (when (empty? body)
      (syntax-error! form ::rule/check-no-body
                     (str what " needs a body. Write an expression that returns true to pass, as in (some? "
                          (or (first args) "x") ").")))
    ;; An unnamed field is usually an unresolved symbol, but a field named like a clojure.core fn (`name`, `count`)
    ;; would resolve to that fn instead, so look for every field by name.
    (when-let [unnamed (seq (filter (set (remove (set args) fields)) (filter symbol? (tree-seq coll? seq body))))]
      (syntax-error! form ::rule/check-uses-unnamed-field
                     (str what " uses fields its argument vector does not name: " (vec (distinct unnamed)) ". A check"
                          " reads only what it names. Add them to the argument vector: "
                          (vec (concat args (distinct unnamed))) ".")))
    (merge {:name    n
            :doc     doc
            :kind    (if external? :external :pure)
            :reads   (mapv keyword args)
            :f       `(fn [~@args ~@(map first pairs)] ~@body)}
           (when (seq pairs) {:fetches (mapv second pairs)}))))

(defn- parse-fetch-clause [fields [_ n reads f :as form]]
  (when-not (qualified-keyword? n)
    (syntax-error! form ::rule/fetch-name-not-qualified
                   (str "the fetch " (pr-str n) ": a fetch's name must be a qualified keyword. Write it like ::"
                        (if (or (keyword? n) (symbol? n)) (name n) "account") ".")))
  (when-not (and (vector? reads) (every? (set fields) reads))
    (syntax-error! form ::rule/fetch-bad-fields
                   (str "the fetch " (pr-str n) " reads " (pr-str reads) ". A fetch needs a vector of fields of the"
                        " draft." (if (vector? reads) (did-you-mean (first (remove (set fields) reads)) fields) "")
                        " Name some of " fields ".")))
  (when-not f
    (syntax-error! form ::rule/fetch-no-fn
                   (str "the fetch " (pr-str n) " has no function. A fetch needs a function of the tuples it fetches."
                        " Write it after the fields, as in (fetch " (pr-str n) " " (pr-str reads) " rows-by-id).")))
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
  parsed type are defined together, so the draft type is the only one that parses to its parsed type.

  The parsed type has no `->Name` factory: only [[parse]] builds a value of it that [[parsed]] accepts."
  [parsed-name fields & clauses]
  (when-not (simple-symbol? parsed-name)
    (syntax-error! &form ::rule/name-not-symbol
                   (str "the name " (pr-str parsed-name) " is not a symbol. A parsed type is a class, named by a"
                        " symbol. Write (defparsed Name [fields] clauses ...), with a symbol for Name.")))
  (check-field-names! &form fields)
  (doseq [c clauses]
    (when-not (and (seq? c) (symbol? (first c)) ('#{pure external fetch child children} (symbol (name (first c)))))
      (syntax-error! c ::rule/unknown-clause
                     (str (pr-str (if (seq? c) (first c) c)) " does not start a clause. A clause is (pure ...),"
                          " (external ...), (fetch ...), (child ...) or (children ...). Use one of those."))))
  (let [clauses     (map #(cons (symbol (name (first %))) (rest %)) clauses)
        by-head     (group-by first clauses)
        fetches     (mapv #(parse-fetch-clause fields %) (get by-head 'fetch))
        fetch-names (set (map :name fetches))
        checks      (mapv #(parse-check-clause fields fetch-names %)
                          (filter (comp '#{pure external} first) clauses))
        children    (into {}
                          (for [[head field type :as c] (concat (get by-head 'child) (get by-head 'children))]
                            (do (when-not (contains? (set fields) field)
                                  (syntax-error! c ::rule/child-not-a-field
                                                 (str "the child field " (pr-str field) " is not a field of the"
                                                      " draft, and a child field must be a field of the draft."
                                                      (did-you-mean field fields) " Name one of " fields ".")))
                                [(keyword field) {:kind (if (= 'child head) :one :many), :type type}])))
        child-heads (map second (concat (get by-head 'child) (get by-head 'children)))
        _           (when-not (apply distinct? ::sentinel child-heads)
                      (syntax-error! &form ::rule/child-declared-twice
                                     (str "a field is declared as a child twice: "
                                          (vec (for [[f c] (frequencies child-heads) :when (< 1 c)] f))
                                          ". Declare it once, with child or children.")))
        names       (concat (map :name checks) (map :name fetches))
        draft-name  (symbol (str parsed-name "Draft"))]
    (when-not (apply distinct? ::sentinel names)
      (syntax-error! &form ::rule/name-used-twice
                     (str "a check or fetch name is used twice: "
                          (vec (for [[x c] (frequencies names) :when (< 1 c)] x))
                          ". A failure is reported under its name. Give each its own name.")))
    (when-let [reserved (seq (filter #(= "parsed-types.child" (namespace %)) names))]
      (syntax-error! &form ::rule/reserved-name
                     (str "names in parsed-types.child are reserved for child-field checks: " (vec reserved)
                          ". Name them in your own namespace, as in ::" (name (first reserved)) ".")))
    `(do
       ~(defparsed-type parsed-name fields)
       ~(defdraft-type draft-name fields)
       ;; through `var`, because `register!` is private: only this expansion may record a plan
       ((var register!) ~parsed-name ~draft-name {:checks ~checks, :fetches ~fetches, :children ~children}
        ~(builders parsed-name draft-name fields))
       ~parsed-name)))

;;; ------------------------------------------------ State ------------------------------------------------

;; Draft type -> its plan, and parsed type -> the same plan, by identity. Written once per class, when its `defparsed`
;; loads. Weak, so the classes a REPL replaces can be collected.
(defonce ^:private ^Map registry
  (.makeMap (.weakKeys (MapMaker.))))

;;; ------------------------------------------------ Plans ------------------------------------------------

(declare plan-for)

(defn- bad-declaration!
  "Throws `::bad-declaration` for the declaration of `draft-class`, which broke `rule`, when its `defparsed` loads."
  [draft-class rule message data]
  (throw (ex-info (str (.getSimpleName ^Class draft-class) ": " message)
                  (merge {:type ::bad-declaration, :rule rule, :draft (.getName ^Class draft-class)} data))))

(defn- closed-over-locals
  "The locals `f` closes over. A check must close over none, or what it reads would not be what it declares."
  [f]
  (into []
        (comp (remove #(Modifier/isStatic (.getModifiers ^Field %)))
              (map #(.getName ^Field %)))
        (.getDeclaredFields (class f))))

(defn- draft-value?
  "Whether `x` is a draft of a type [[defparsed]] defined. A type that only implements `DraftValue` is not."
  [x]
  (and (instance? DraftValue x) (some? (.get registry (class x)))))

(defn- validate-declaration!
  "Throw `::bad-declaration` for what `defparsed` cannot see when it expands, only when it loads: a child type that is
  not a parsed type or a sum, a fetch that is not a fn, and a pure check that closes over a local."
  [draft-class {:keys [checks fetches children]}]
  (doseq [[field {:keys [kind type]}] children]
    (when-not (or (parsed-type? type) (sum? type))
      (bad-declaration! draft-class ::rule/child-type-not-parsed
                        (str "the child field " field " holds " (pr-str type) ", which is not a type defined with"
                             " defparsed or a sum defined with defsum, in (" (if (= :one kind) "child" "children") " "
                             (name field) " " (pr-str type) "). A child field holds drafts the parent parses. Name"
                             " one of those.")
                        {:field field, :got (pr-str type)})))
  (doseq [{n :name, :keys [f reads]} fetches]
    (when-not (fn? f)
      (bad-declaration! draft-class ::rule/fetch-not-fn
                        (str "the fetch " n " is " (pr-str f) ", not a fn, in (fetch " n " " (mapv symbol reads) " "
                             (pr-str f) "). A fetch is called with the tuples it fetches. Write a fn of the tuples"
                             " it fetches, as in (fetch " n " " (mapv symbol reads) " rows-by-id).")
                        {:name n, :got (some-> f class .getName)})))
  (doseq [{n :name, :keys [kind f reads]} checks]
    (when-let [locals (and (= :pure kind) (seq (closed-over-locals f)))]
      (bad-declaration! draft-class ::rule/pure-check-closes-over-locals
                        (str "the pure check " n " closes over locals " (mapv symbol locals) ", in (pure " n " ... "
                             (mapv symbol reads) " ...). A pure check reads only the fields it names. Read the value"
                             " from a field, or make the check external.")
                        {:name n, :locals locals}))))

(defn- children-of-type?
  "Whether `v` is what a child field, of `kind` and `parsed-type`, may hold. A draft's parsed type is compared by name,
  so after a REPL re-evaluates a child type, a parent defined before it accepts drafts of the new class."
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
  [parses-to draft-class fields {:keys [checks fetches children], :as decl}]
  (validate-declaration! draft-class decl)
  (let [shape     (into {} (map (fn [[field child]] [field (child-shape-check field child)])) children)
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
     :fields          fields
     :children        children
     :pure            (vec (sort-by (comp count reads-set) pure'))
     :fetches         (vec fetches')
     :external        (vec external')
     :checks-by-name  (into {} (map (juxt :name identity)) (concat pure' external'))}))

(defn- describe
  "`x` for an error message: nil, a plain map, or a value of its type."
  [x]
  (cond
    (nil? x)                                            "nil"
    (and (map? x) (not (instance? ParsedValue x))
         (not (instance? DraftValue x)))                "a plain map"
    :else                                               (str "a " (.getSimpleName (class x)))))

(defn- register!
  "Build and check the plan of `draft-type`, which parses to `parsed-type`, and record it for both, with the
  `builders` [[defparsed]] generated. Called by the expansion of [[defparsed]], when it loads; throws
  `::bad-declaration` then."
  [parsed-type draft-type decl builders]
  (let [plan (merge (compile-plan parsed-type draft-type (:fields builders) decl) builders)]
    (.put registry draft-type plan)
    (.put registry parsed-type plan)))

(defn- plan-for [draft]
  (when-not (draft-value? draft)
    (throw (ex-info (str "pt/parse got " (describe draft) ". It parses only drafts of types defparsed defined."
                         (cond
                           (instance? ParsedValue draft)
                           " It is already parsed: pass it as it is, or call (pt/draft-of x) to get its draft."

                           (map? draft)
                           " Build a draft from the map with (pt/draft Name m), as in (pt/parse (pt/draft Debit m))."

                           :else
                           " Build one with its constructor, as in (pt/parse (->DebitDraft ...)), or with pt/draft."))
                    {:type ::bad-input, :rule ::rule/not-a-draft, :got (some-> draft class .getName)})))
  (.get registry (class draft)))

(defn declaration
  "The declaration `defparsed` wrote for `draft`'s type: `{:parses-to :checks :fetches :children}`, as data. Throws
  `::bad-input` for anything but a draft."
  [draft]
  (:declaration (plan-for draft)))

;;; ------------------------------------------------ Running ------------------------------------------------

(declare issued?)

(def ^:private immutable-classes
  "Classes whose values never change, by exact class: `BigDecimal` and `BigInteger` can be subclassed, so a subclass is
  not trusted."
  #{Boolean Character String clojure.lang.Keyword clojure.lang.Symbol java.util.UUID java.util.regex.Pattern
    Long Integer Short Byte Double Float BigDecimal BigInteger clojure.lang.BigInt clojure.lang.Ratio})

(def ^:private max-value-depth
  "The most levels of nesting a field's value may have. A deeper one throws `::bad-input`."
  1000)

(defn- java-time-value?
  "Whether `c` is a final class of the `java.time` package, such as `Instant`, `LocalDate` or `ZoneOffset`: the JDK
  documents them as immutable. `java.time.format`, `java.time.temporal` and the rest are other packages."
  [^Class c]
  (and (= "java.time" (.getPackageName c)) (Modifier/isFinal (.getModifiers c))))

(defn- not-immutable
  "nil when `v` is known to be immutable all the way down, else `[path what fix class-name]`: where inside the field
  the first value that is not is, what it is, the fix, and its class. Accepts nil, the values of
  [[immutable-classes]], classes, `java.time` values, values [[parse]] issued, and persistent vectors, maps (records
  and drafts included), sets, lists and queues whose contents pass, map keys included. Refuses everything else, lazy
  and other seqs too: realizing one could run side effects or never end."
  [v path]
  (let [c       (class v)
        refuse  (fn [what fix] [path what fix (.getName ^Class c)])]
    (cond
      (< max-value-depth (count path))           (refuse (str "a value nested deeper than " max-value-depth " levels")
                                                         "Flatten it.")
      (nil? v)                                   nil
      (contains? immutable-classes c)            nil
      (class? v)                                 nil
      (java-time-value? c)                       nil
      (issued? v)                                nil
      (instance? clojure.lang.IPersistentMap v)  (some (fn [[k x]] (or (not-immutable k (conj path '<key>))
                                                                       (not-immutable x (conj path k))))
                                                       v)
      (or (instance? clojure.lang.IPersistentVector v) (list? v)
          (instance? clojure.lang.PersistentQueue v)) (some (fn [[i x]] (not-immutable x (conj path i)))
                                                            (map-indexed vector v))
      (instance? clojure.lang.IPersistentSet v)  (some #(not-immutable % (conj path '<element>)) v)
      (.isArray c)                               (refuse "a Java array" "Copy it into a vector, as in (vec a).")
      (instance? clojure.lang.IAtom v)           (refuse "an atom" "Deref it, as in @a.")
      (instance? clojure.lang.IRef v)            (refuse "a reference type" "Deref it, as in @r.")
      (instance? clojure.lang.Volatile v)        (refuse "a volatile" "Deref it, as in @v.")
      (instance? java.util.Date v)               (refuse "a mutable date"
                                                         "Use java.time.Instant, as in (.toInstant d).")
      (instance? clojure.lang.LazySeq v)         (refuse (str "a lazy seq, which realizing could run side effects"
                                                              " or never end")
                                                         "Pass a vector, as in (vec s).")
      (instance? clojure.lang.ISeq v)            (refuse "a seq, which may be lazy" "Pass a vector, as in (vec s).")
      (or (instance? java.util.Collection v)
          (instance? java.util.Map v))           (refuse "a mutable Java collection"
                                                         (str "Copy it into a Clojure collection, as in (vec c)"
                                                              " or (into {} m)."))
      :else                                      (refuse "a value not known to be immutable"
                                                         (str "Convert it to an immutable value, such as a"
                                                              " string, a number, a java.time value or a"
                                                              " persistent collection.")))))

(defn- refuse-mutable!
  "Throws `::bad-input` when a field of `draft` holds, anywhere inside it, a value that is not known to be immutable:
  the checks would prove a value the parsed value does not keep. Child fields are not walked, because each child is
  checked as its own node. There is no opt-out."
  [draft {:keys [children]} values]
  (doseq [[field v] values
          :when       (not (contains? children field))
          :let        [[path what fix got] (not-immutable v [])]
          :when       what]
    (throw (ex-info (str (.getSimpleName (class draft)) ": the field " field (when (seq path) (str " at " path))
                         " holds " what " (" got "). A parsed value holds only values known to be immutable, so what"
                         " its checks proved stays true. " fix)
                    {:type ::bad-input, :rule ::rule/mutable-value, :draft (.getName (class draft)), :field field
                     :path path, :got got}))))

(defn- start [draft]
  (let [plan   (plan-for draft)
        ;; every field read once, so the checks and the parsed value see the same values
        values ((:read-draft plan) draft)]
    (refuse-mutable! draft plan values)
    {:draft   draft
     :values  values
     :plan    plan
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

(defn- too-big! [rule message data]
  (throw (ex-info message (merge {:type ::bad-input, :rule rule, :max-depth max-depth, :max-nodes max-nodes} data))))

(defn- flatten-tree
  "The drafts of the tree under `root`, children before parents, each with its `:path` from the root."
  [tree root]
  (loop [stack (list [root [] 1]), pre []]
    (if-let [[draft path depth] (first stack)]
      (let [_    (when (< max-depth depth)
                   (too-big! ::rule/tree-too-deep
                             (str "The tree under a " (.getSimpleName (class root)) " is deeper than " max-depth
                                  " levels, counting the root. A tree is walked with a bounded stack."
                                  " Parse a shallower tree, or split it and parse the parts.")
                             {:path path}))
            _    (when (<= max-nodes (count pre))
                   (too-big! ::rule/tree-too-big
                             (str "The tree under a " (.getSimpleName (class root)) " holds more than " max-nodes
                                  " drafts, counting a draft each time it appears. A parse is bounded in size. Parse"
                                  " it in smaller parts, as with (pt/parse-all parts).")
                             {}))
            st   (assoc (start draft) :tree tree :path path)
            kids (for [[field {:keys [kind type]}] (get-in st [:plan :children])
                       :let  [v (get-in st [:values field])]
                       :when (children-of-type? kind type v)
                       [i d] (if (= :one kind) (when v [[nil v]]) (map-indexed vector v))]
                   [field (child-path path field i) d])
            st   (assoc st :child-paths (reduce (fn [m [field p]] (update m field (fnil conj []) p)) {} kids))]
        ;; pushing the children in order pops the last first, so the reverse of this pre-order is a post-order with
        ;; the children in order
        (recur (into (rest stack) (map (fn [[_ p d]] [d p (inc depth)])) kids) (conj pre st)))
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
          checks))

(defn- run-pure [st bad]
  (run-checks st (get-in st [:plan :pure]) bad))

(declare build-parsed)

(defn- child-inputs
  "The node's field values, with each child field holding `(built path)` for the path of each of its children."
  [{:keys [plan values child-paths]} built]
  (reduce-kv (fn [m field paths]
               (assoc m field (if (= :one (get-in plan [:children field :kind]))
                                (built (first paths))
                                (mapv built paths))))
             values child-paths))

(defn- pure-phase
  "Run every node's pure checks, children before parents, and build each node's view: a value of its parsed type,
  without the proof, holding its children's views. A parent's checks see each child as its view, which is closed like
  any parsed value, and which `pt/parsed` refuses. A node is bad when it, or anything under it, failed."
  [nodes]
  (first (reduce (fn [[out bad views] {:keys [tree path child-paths plan], :as st}]
                   (let [inputs (child-inputs st #(views [tree %]))
                         view   (build-parsed plan inputs nil)
                         st     (run-pure (assoc st :inputs inputs) bad)
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
                     fd     (get-in st [:plan :fetches])]
                 {:i i, :fd fd})
        tuple  (fn [{:keys [i fd]}] (mapv #(get (:inputs (nth sts i)) %) (:reads fd)))]
    (reduce
     (fn [sts [_ ws]]
       (let [{fd :fd} (first ws)
             tuples   (into [] (comp (map tuple) (distinct)) ws)
             result   ((:f fd) tuples)]
         (when-let [missing (seq (remove #(and (map? result) (contains? result %)) tuples))]
           (throw (ex-info (if (map? result)
                             (str "Fetch " (:name fd) " returned no entry for " (vec missing) ". A fetch answers every"
                                  " tuple it is given, so no check reads a nil it did not mean. Return a key for every"
                                  " tuple, with nil for not found, as in (into {} (map (fn [t] [t (lookup t)]))"
                                  " tuples).")
                             (str "Fetch " (:name fd) " returned " (describe result) ", not a map. A fetch returns a"
                                  " map from each tuple to what it fetched. Return a map with a key for every tuple,"
                                  " with nil for not found, as in (into {} (map (fn [t] [t (lookup t)])) tuples)."))
                           {:type    ::bad-fetch
                            :rule    (if (map? result) ::rule/fetch-missing-tuples ::rule/fetch-not-a-map)
                            :fetch   (:name fd)
                            :missing (vec missing)})))
         (reduce (fn [sts w]
                   (assoc-in sts [(:i w) :fetched (:name fd)] (get result (tuple w))))
                 sts ws)))
     sts
     ;; one call per fetch per plan: the same fetch name in two declarations is two fetches
     (group-by (fn [{:keys [i fd]}] [(get-in sts [i :plan]) (:name fd)]) wanted))))

(defn- run-all
  "Run the checks of every draft, and of every draft under it, and return one vector of nodes per draft."
  [drafts]
  (let [nodes        (pure-phase (into [] (comp (map-indexed (fn [i d] (flatten-tree i d))) cat) drafts))
        unproven     (into #{} (comp (filter (comp seq :failed)) (map :tree)) nodes)
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
  "Each check that returned a failure in `nodes`, in the order they ran, as `[label doc]`, the label naming its path
  when it is under the root."
  [nodes]
  (for [{:keys [path failed order]} nodes
        n                           order
        :when                       (contains? (failed n) :returned)]
    [(str n (when (seq path) (str " at " path))) (:doc (failed n))]))

(defn- build-parsed
  "A value of the parsed type of `plan` with the fields in `values`, holding `proof`."
  [plan values proof]
  ((:make-parsed plan) values proof))

(defn- issue-tree
  "The parsed root of the tree that passed: every node rebuilt with the proof, children before parents, so each parent
  holds its children's issued values. The checks saw the same fields, in values built without it."
  [nodes]
  (let [issued (reduce (fn [built {:keys [path plan], :as st}]
                         (assoc built path (build-parsed plan (child-inputs st built) parse-token)))
                       {}
                       nodes)]
    (get issued [])))

(defn- invalid!
  "Throws `::invalid` for `what`, naming each failed check of `named`, `[label doc]` pairs, and its doc: the first
  five, then how many more."
  [what named nodes data]
  (let [shown (take 5 named)
        more  (- (count named) (count shown))]
    (throw (ex-info (str "Invalid " what ": " (count named) (if (= 1 (count named)) " check" " checks") " failed: "
                         (clojure.string/join ", " (map (fn [[label doc]] (str label " (" doc ")")) shown))
                         (when (pos? more) (str ", and " more " more"))
                         ". A draft parses only when every check returns exactly true. Change the draft so they pass"
                         " and parse again; (:failures (ex-data e)) has what each returned.")
                    (merge {:type ::invalid, :rule ::rule/checks-failed, :failed-kinds (failed-kinds nodes)} data)))))

;;; ------------------------------------------------ API ------------------------------------------------

(defn assoc-draft
  "`draft` with `field` set to `v`: a new draft. This is what `assoc` on a draft does; call `assoc`."
  [draft field v]
  (let [{:keys [fields make-draft read-draft]} (plan-for draft)]
    (when-not (some #{field} fields)
      (no-field! draft field "assoc"))
    (make-draft (assoc (read-draft draft) field v))))

(defn parse-all
  "Parse each draft and the drafts under it. Returns a vector of parsed values aligned with `drafts`, or throws
  `::invalid` once with every failure: `:failures` maps the index of each draft that failed to its failures, shaped
  like [[parse]]'s, and `:failed-kinds` holds the kinds of check that failed in any of them. Exceptions out of a check
  or fetch propagate, so an external check that throws stops the call."
  [drafts]
  (let [trees    (run-all drafts)
        failures (into (sorted-map) (keep-indexed (fn [i nodes] (some->> (tree-failures nodes) (vector i)))) trees)]
    (when (seq failures)
      (let [failing (map #(nth trees %) (keys failures))]
        (invalid! (str "drafts at indexes " (vec (keys failures)))
                  (for [i (keys failures), [label doc] (failed-names (nth trees i))] [(str label " in draft " i) doc])
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
  (let [nodes (first (run-all [draft]))]
    (when-let [failures (tree-failures nodes)]
      (invalid! (.getSimpleName ^Class (:parses-to (:plan (peek nodes)))) (failed-names nodes) nodes
                {:draft (.getName (class draft)), :failures failures}))
    (issue-tree nodes)))

(defn- issued?
  "Whether `x` is a parsed value that holds the proof: one [[parse]] built. Its class must be one [[defparsed]]
  registered, by identity, before `x` is asked about the proof: a class written by hand, even with the same name,
  never sees the token."
  [x]
  (and (instance? ParsedValue x)
       (identical? (class x) (:parses-to (.get registry (class x))))
       (true? (.issuedWith ^ParsedValue x parse-token))))

(defn parsed?
  "Whether `x` is a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`: a value of
  a class [[defparsed]] defined, whose hidden proof field holds the proof only `parse` sets. Which type `x` counts as
  is decided by class name, so a value of a class a REPL re-evaluated counts as the type."
  [parsed-type x]
  (and (some? x)
       (member? parsed-type (class x))
       (issued? x)))

(defn- parse-hint
  "The code that makes a parsed value of `type`, for an error message."
  [type]
  (let [one (fn [^Class c] (str "(pt/parse (->" (if-let [plan (.get registry c)]
                                                  (.getSimpleName ^Class (:draft-class plan))
                                                  (str (.getSimpleName c) "Draft"))
                                " ...))"))]
    (if (sum? type)
      (clojure.string/join " or " (map one (:members type)))
      (one type))))

(defn parsed
  "`x`, which must be a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`. Throws
  `::not-parsed` otherwise. Read its fields by keyword, as in `(:amount x)`: a key that is not a field throws
  `::no-field`."
  [parsed-type x]
  (when-not (parsed? parsed-type x)
    (let [t                      (type-name parsed-type)
          {:keys [rule message]} (cond
                                   (nil? x)
                                   {:rule    ::rule/got-nil
                                    :message (str "Expected a parsed " t ", got nil. A sink takes only a value"
                                                  " pt/parse issued. Write " (parse-hint parsed-type)
                                                  " and pass the result.")}

                                   (and (draft-value? x) (member? parsed-type (:parses-to (plan-for x))))
                                   {:rule    ::rule/got-draft
                                    :message (str "Expected a parsed " t ", got " (describe x) ", which is not"
                                                  " checked yet. A sink takes only a value pt/parse issued. Call"
                                                  " pt/parse on it first, as in (pt/parse draft), and pass the"
                                                  " result.")}

                                   (member? parsed-type (class x))
                                   {:rule    ::rule/got-unissued
                                    :message (str "Expected a parsed " t ", got " (describe x) " that pt/parse did"
                                                  " not issue. A value built with its constructor or by hand"
                                                  " carries no proof. Write " (parse-hint parsed-type)
                                                  " and pass the result.")}

                                   (and (map? x) (not (instance? ParsedValue x)) (not (instance? DraftValue x)))
                                   {:rule    ::rule/got-plain-map
                                    :message (str "Expected a parsed " t ", got a plain map. A sink takes only a"
                                                  " value pt/parse issued. Build a draft from the map and parse it,"
                                                  " as in (pt/parse (pt/draft "
                                                  (if (sum? parsed-type) (type-name (first (:members parsed-type))) t)
                                                  " m)), and pass the result.")}

                                   :else
                                   {:rule    ::rule/got-other-type
                                    :message (str "Expected a parsed " t ", got " (describe x) ". A sink takes only"
                                                  " a value of its type. Pass a " t " instead: "
                                                  (parse-hint parsed-type) ".")})]
      (throw (ex-info message
                      {:type     ::not-parsed
                       :rule     rule
                       :expected (if (sum? parsed-type) (str (:name parsed-type)) (.getName ^Class parsed-type))
                       :got      (some-> x class .getName)}))))
  x)

(defn draft-of
  "A draft with the fields of `parsed`, a value [[parse]] issued. Child fields hold drafts of the children. Throws
  `::not-parsed` for any other value."
  [parsed]
  (let [plan (when (issued? parsed) (.get registry (class parsed)))]
    (when-not plan
      (throw (ex-info (if (instance? DraftValue parsed)
                        (str "pt/draft-of got " (describe parsed) ", which is already a draft. draft-of turns a value"
                             " pt/parse issued back into a draft. Use the draft as it is.")
                        (str "pt/draft-of got " (describe parsed)
                             (when (instance? ParsedValue parsed) " that pt/parse did not issue")
                             ". draft-of takes only a value pt/parse issued. Build a draft with its constructor"
                             " instead, as in (->DebitDraft ...), or with (pt/draft Debit m)."))
                      {:type ::not-parsed
                       :rule (if (instance? DraftValue parsed) ::rule/draft-of-a-draft ::rule/draft-of-unissued)
                       :got  (some-> parsed class .getName)})))
    (let [values (reduce-kv (fn [vs field {:keys [kind]}]
                              (update vs field #(if (= :one kind) (some-> % draft-of) (mapv draft-of %))))
                            (into {} (map (fn [f] [f (get parsed f)])) (:fields plan))
                            (:children plan))]
      ((:make-draft plan) values))))

;;; ------------------------------------------------ Building and reading ------------------------------------------------

(defn draft-from-map
  "A draft of `draft-type` with the fields in `m`, nil for each one `m` leaves out. A non-map, or a key that is not a
  field, throws `::bad-input`. Called by [[draft]]."
  [^Class draft-type m]
  (let [{:keys [fields make-draft]} (.get registry draft-type)]
    (when-not (map? m)
      (throw (ex-info (str "pt/draft got " (describe m) " to build a " (.getSimpleName draft-type) ". A draft is"
                           " built from a map of its fields. Pass a map with some of its fields " fields ", as in {"
                           (first fields) " ...}.")
                      {:type ::bad-input, :rule ::rule/draft-from-non-map, :got (some-> m class .getName)})))
    (when-let [unknown (seq (remove (set fields) (keys m)))]
      (throw (ex-info (str (.getSimpleName draft-type) " has no fields " (vec unknown) "."
                           (did-you-mean (first unknown) fields) " A draft holds only its fields, so a misspelled"
                           " key is never dropped. Remove them, or use its fields " fields ".")
                      (cond-> {:type           ::bad-input
                               :rule           ::rule/draft-unknown-keys
                               :unknown-fields (vec unknown)
                               :fields         fields}
                        (nearest (first unknown) fields) (assoc :suggestion (nearest (first unknown) fields))))))
    (make-draft m)))

(defmacro draft
  "A draft of the parsed type `parsed-type`, defined with [[defparsed]], with the fields in `m`.

  When `m` is a literal map, it must name every field, and only fields: anything else is a compile error, so write
  `nil` for a field you mean to leave empty. A map built at run time, usually a request, may leave fields out, which
  are nil; a key that is not a field throws `::bad-input`.

    (pt/draft Debit {:user-id u, :account-id 1, :amount 30M})"
  [parsed-type m]
  (let [refuse! (fn [rule message data] (throw (ex-info message (merge {:type ::bad-input, :rule rule} data))))
        cls     (resolve parsed-type)
        dcls    (when (and (parsed-type? cls) (.get registry cls)) (:draft-class (.get registry cls)))
        _       (when-not dcls
                  (refuse! ::rule/draft-not-a-parsed-type
                           (str "pt/draft: " parsed-type " is not a type defined with defparsed. pt/draft builds the"
                                " draft of a parsed type. Name a parsed type, not a sum or a draft, as in (pt/draft"
                                " Debit {...}).")
                           {:got (str parsed-type)}))
        dsym    (symbol (.getName ^Class dcls))
        flds    (:fields (.get registry cls))]
    (if (map? m)
      (do (when-let [unknown (seq (remove (set flds) (keys m)))]
            (refuse! ::rule/draft-literal-unknown-keys
                     (str "pt/draft: " parsed-type " has no fields " (vec unknown) "."
                          (did-you-mean (first unknown) flds) " A literal map names only fields; its fields are "
                          flds ". Remove the others.")
                     {:unknown-fields (vec unknown), :fields flds}))
          (when-let [missing (seq (remove (set (keys m)) flds))]
            (refuse! ::rule/draft-literal-missing-fields
                     (str "pt/draft: " parsed-type " needs every field; missing " (vec missing) ". A literal map names"
                          " every field, so none is left out by mistake. Write nil for a field you mean to leave"
                          " empty, as in " (first missing) " nil.")
                     {:missing (vec missing), :fields flds}))
          ;; bind in the order written, so the values are evaluated as they read
          (let [syms (into {} (map (fn [k] [k (gensym (name k))])) (keys m))]
            `(let [~@(mapcat (fn [[k v]] [(syms k) v]) m)]
               (new ~dsym ~@(map syms flds)))))
      `(draft-from-map ~dsym ~m))))
