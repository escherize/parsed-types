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

  A `pure` check is one that should read only its arguments; an `external` one reads the world. The library holds a
  check to the fields it names and refuses field values that can change, but it cannot stop a pure check from
  calling the world. What it guarantees: nothing fetches and no external check runs until every pure check in the
  tree passes.

  A field declared with `children` or `child` holds drafts, and one [[parse]] parses the whole tree.

  Drafts and parsed values are maps with fixed fields: reading a key that is not a field throws. A parsed value cannot
  change: to change one, take [[draft-of]] it, `assoc`, and [[parse]] again.

  A parsed value carries its proof: a hidden field that only [[parse]] sets. So [[parsed]] needs no registry of
  issued values, and a value built by hand, equal or not, is refused.

  Parsed types compare by class, like records: when a REPL re-evaluates a `defparsed`, it makes new classes, and code
  evaluated against the old ones keeps them. Where a class does not match but its name does, the library throws
  `::stale-type`, naming what to re-evaluate.

  Every error the library throws is an `ex-info` whose ex-data has a `:type` in this namespace: `::invalid` (checks
  failed), `::not-parsed`, `::stale-type`, `::cannot-change`, `::no-field`, `::bad-input`, `::bad-declaration` or
  `::bad-fetch`, and
  a `:rule` in `parsed-types.rule` that names the rule it broke. Its message says what happened, with the values, the
  rule, and the fix."
  (:require
   [clojure.set :as set]
   [parsed-types.rule :as-alias rule]
   [clojure.string])
  (:import
   (com.google.common.collect MapMaker)
   (java.lang.reflect Modifier)
   (java.util Map)))

(set! *warn-on-reflection* true)

(defn- class-loaded?
  "Whether the class named `class-name` is already loaded."
  [class-name]
  (try (Class/forName class-name false (clojure.lang.RT/baseLoader))
       true
       (catch ClassNotFoundException _ false)))

(defmacro ^:private defclass-once
  "`form`, a `definterface` or `defrecord` of this namespace, unless the class it names is already loaded: then only
  an import of it. Reloading this namespace then keeps its classes, so every value and type defined before still
  implements them. When compiling to class files, `form` always runs, so the class file is written."
  [form]
  (let [class-name (str (namespace-munge *ns*) "." (second form))]
    (if (and (not *compile-files*) (class-loaded? class-name))
      `(import ~(symbol class-name))
      form)))

(defclass-once
  (definterface ParsedValue
    ;; Whether the value's proof is `t`. The proof goes in and only a boolean comes out, so nothing hands the proof
    ;; out.
    (issuedWith [t])))

;; The proof that [[parse]] issued a value: a parsed value is parsed exactly when its hidden `parse-proof` field holds
;; this object. Only `parse` builds a value with it. `defonce`, so reloading this namespace keeps the values already
;; issued parsed.
(defonce ^:private parse-token (Object.))

(defclass-once
  (definterface DraftValue))

;;; ------------------------------------------------ State ------------------------------------------------

;; Draft type -> its plan, and parsed type -> the same plan, by identity. Written once per class, when its `defparsed`
;; loads. Weak, so the classes a REPL replaces can be collected.
(defonce ^:private ^Map registry
  (.makeMap (.weakKeys (MapMaker.))))

;; Class name -> `{:ns :role :type}`: the namespace, as written, whose `defparsed` first claimed it, the role the class
;; has there, `:parsed` or `:draft`, and the parsed type's name. `my-app.bank` and `my_app.bank` compile to the same
;; Java names, and a type named `FooDraft` has the name of `Foo`'s draft, so without this one `defparsed` could
;; redefine the classes of another.
(defonce ^:private ^java.util.concurrent.ConcurrentHashMap owners
  (java.util.concurrent.ConcurrentHashMap.))

;; Parsed class name -> the fetch fns and child types its `defparsed` evaluated and checked before defining any class,
;; for `register!` to take once the classes exist. A failed `defparsed` leaves its entry, which the next one replaces.
(defonce ^:private ^java.util.concurrent.ConcurrentHashMap pending
  (java.util.concurrent.ConcurrentHashMap.))

;; Parsed class name -> the class its `defparsed` defined last. A class that has the name but is not this one is from
;; before a REPL re-evaluated the `defparsed`: stale.
(defonce ^:private ^java.util.concurrent.ConcurrentHashMap latest
  (java.util.concurrent.ConcurrentHashMap.))

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

(defn- library-value!
  "Throws `::bad-input` unless `x` is a draft or a parsed value. `fn-name` names the fn that got it, which the types
  [[defparsed]] defines call."
  [fn-name x]
  (when-not (or (instance? DraftValue x) (instance? ParsedValue x))
    (throw (ex-info (str fn-name " got " (pr-str x) ", which is not a draft or parsed value. It reports a change or a"
                         " missing field of a type defparsed defined, and the types call it. Read a missing key of"
                         " your own map with (get m k default), and change it with assoc.")
                    {:type ::bad-input, :rule ::rule/not-a-library-value, :fn fn-name
                     :got (some-> x class .getName)}))))

(declare no-field!)

(defn- final!
  "Throws `::cannot-change`: a parsed value cannot be changed. `k` is the key `op` was given, or nil. A key that is
  not a field throws `::no-field` first, as [[no-field!]] says. Called by the parsed types [[defparsed]] defines."
  [parsed op k]
  (library-value! "parsed-types.core/final!" parsed)
  (when (and (some? k) (not (contains? parsed k)))
    (no-field! parsed k op))
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
                                  :message (str op (when (some? k) (str " " (pr-str k))) " on a " n ": a parsed value"
                                                " never changes, so its checks stay true. Use (pt/parse (assoc"
                                                " (pt/draft-of x) " (if (some? k) (pr-str k) ":field")
                                                (if (#{"dissoc" "remove"} op) " nil" " v") ")) for a changed copy.")})]
    (throw (ex-info message {:type ::cannot-change, :rule rule, :parsed (.getName (class parsed)), :op op, :key k}))))

(defn- fixed!
  "Throws `::cannot-change`: a draft's fields are fixed. `k` is the key `op` was given, or nil. A key that is not a
  field throws `::no-field` first. Called by the draft types [[defparsed]] defines."
  [draft op k]
  (library-value! "parsed-types.core/fixed!" draft)
  (when (and (some? k) (not (contains? draft k)))
    (no-field! draft k op))
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
                                  :message (str op (when (some? k) (str " " (pr-str k))) " on a " n ": a draft has"
                                                " fixed fields, set with assoc. Use (assoc d "
                                                (if (some? k) (pr-str k) ":field")
                                                (if (#{"dissoc" "remove"} op) " nil" " v") ").")})]
    (throw (ex-info message {:type ::cannot-change, :rule rule, :draft (.getName (class draft)), :op op, :key k}))))

(defn- no-field!
  "Throws `::no-field`: `x` has no field `k`. `op` is `\"read\"` when a key that is not a field is read without a
  default, or the change, such as `\"assoc\"` or `\"dissoc\"`, that named it. Called by the types [[defparsed]]
  defines."
  [x k op]
  (library-value! "parsed-types.core/no-field!" x)
  (let [fields  (vec (keys x))
        n       (.getSimpleName (class x))
        draft?  (instance? DraftValue x)
        nearest (nearest k fields)
        field   (pr-str (or nearest (first fields)))]
    (throw (ex-info (cond
                      (empty? fields)
                      (str n " has no field " (pr-str k) ": it has no fields at all, so every key "
                           (if (= "read" op) "read without a default throws" "throws") ". Add the field to its"
                           " defparsed, as in (defparsed " (if draft? (subs n 0 (- (count n) 5)) n) " ["
                           (if (keyword? k) (name k) "field") "]).")

                      (= "read" op)
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A key that is not a field"
                           " throws, never reads nil. Read one of its fields " fields ", as in (" field " x).")

                      draft?
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A draft has fixed fields. Set"
                           " one of " fields ", as in (assoc d " field " v).")

                      :else
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A parsed value has fixed"
                           " fields, and never changes. Change one of " fields " on a copy, as in (pt/parse (assoc"
                           " (pt/draft-of x) " field " v))."))
                    (cond-> {:type                     ::no-field
                             :rule                     (if (= "read" op)
                                                         ::rule/no-such-field-read
                                                         ::rule/no-such-field-assoc)
                             (if draft? :draft :parsed) (.getName (class x))
                             :key                      k
                             :op                       op
                             :fields                   fields}
                      nearest (assoc :suggestion nearest))))))

(defn- not-a-pair!
  "Throws `::bad-input`: `(conj x v)` with a vector `v` that is not a `[key value]` pair. Called by the types
  [[defparsed]] defines."
  [x v]
  (throw (ex-info (str "conj of " (pr-str v) " on a " (.getSimpleName (class x)) ": a map takes a [key value] pair, and"
                       " this vector has " (count v) " elements. Write (conj x [k v]), or (assoc x k v).")
                  {:type ::bad-input, :rule ::rule/conj-not-a-pair, :got v})))

(defn- read-only-entry
  "A `java.util.Map$Entry` of `e`, an entry of `x`, a draft or parsed value, whose `setValue` throws as `fail`, the
  [[final!]] or [[fixed!]] of `x`, does. Called by the types [[defparsed]] defines."
  [x fail ^java.util.Map$Entry e]
  (let [k (.getKey e), v (.getValue e)]
    (reify java.util.Map$Entry
      (getKey [_] k)
      (getValue [_] v)
      (setValue [_ _] (fail x "setValue" k))
      ;; as java.util.Map$Entry specifies
      (equals [_ o] (and (instance? java.util.Map$Entry o)
                         (java.util.Objects/equals k (.getKey ^java.util.Map$Entry o))
                         (java.util.Objects/equals v (.getValue ^java.util.Map$Entry o))))
      (hashCode [_] (bit-xor (java.util.Objects/hashCode k) (java.util.Objects/hashCode v)))
      (toString [_] (str k "=" v)))))

(defn- read-only-view
  "The `java.util` view `view`, `\"keySet\"`, `\"values\"` or `\"entrySet\"`, of `x`, a draft or parsed value, over
  the vector `items`: a `Set`, or for `values` a `Collection`. Each mutator, of the view or of its iterator, throws as
  `fail`, the [[final!]] or [[fixed!]] of `x`, does. Called by the types [[defparsed]] defines."
  [x fail view items]
  (let [refuse (fn [op] (fail x (str view "()." op) nil))
        values? (= "values" view)
        ^java.util.Collection back (if values?
                                     (java.util.ArrayList. ^java.util.Collection items)
                                     (java.util.LinkedHashSet. ^java.util.Collection items))
        iter   (fn []
                 (let [it (.iterator back)]
                   (reify java.util.Iterator
                     (hasNext [_] (.hasNext it))
                     (next [_] (.next it))
                     (remove [_] (refuse "iterator().remove")))))]
    ;; a Set, so a key set or entry set equals a set with the same members; values is only a Collection, as for
    ;; java.util.HashMap, equal only to itself
    (if values?
      (reify java.util.Collection
        (size [_] (.size back))
        (isEmpty [_] (.isEmpty back))
        (contains [_ o] (.contains back o))
        (containsAll [_ c] (.containsAll back c))
        (iterator [_] (iter))
        (toArray [_] (.toArray back))
        (^objects toArray [_ ^objects a] (.toArray back a))
        (add [_ _] (refuse "add"))
        (addAll [_ _] (refuse "addAll"))
        (remove [_ _] (refuse "remove"))
        (removeAll [_ _] (refuse "removeAll"))
        (retainAll [_ _] (refuse "retainAll"))
        (removeIf [_ _] (refuse "removeIf"))
        (clear [_] (refuse "clear"))
        (toString [_] (.toString back)))
      (reify java.util.Set
        (size [_] (.size back))
        (isEmpty [_] (.isEmpty back))
        (contains [_ o] (.contains back o))
        (containsAll [_ c] (.containsAll back c))
        (iterator [_] (iter))
        (toArray [_] (.toArray back))
        (^objects toArray [_ ^objects a] (.toArray back a))
        (add [_ _] (refuse "add"))
        (addAll [_ _] (refuse "addAll"))
        (remove [_ _] (refuse "remove"))
        (removeAll [_ _] (refuse "removeAll"))
        (retainAll [_ _] (refuse "retainAll"))
        (removeIf [_ _] (refuse "removeIf"))
        (clear [_] (refuse "clear"))
        (equals [_ o] (.equals back o))
        (hashCode [_] (.hashCode back))
        (toString [_] (.toString back))))))

(defn- map-type-methods
  "The interfaces and methods of a read-only map with exactly `fields`. Reading another key throws unless a default
  is given. `assoc-form`, over `this`, `k` and `v`, is what `assoc` does; `final-name`, the form of a fn, throws for
  any other change.
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
           ;; through `var`, because it is private
                  ((var not-a-pair!) ~this ~o))
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
      (~'valAt [~this ~k] ~(lookup `((var no-field!) ~this ~k "read")))
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
      ;; views whose mutators, and their iterators' and entries', throw like the map's own
      (~'keySet [~this] ((var read-only-view) ~this ~final-name "keySet" ~kws))
      (~'values [~this] ((var read-only-view) ~this ~final-name "values" [~@fields]))
      (~'entrySet [~this] ((var read-only-view) ~this ~final-name "entrySet"
                                                (clojure.core/mapv (fn [e#] ((var read-only-entry) ~this ~final-name e#))
                                                                   ~entries)))
      ;; ex-info, a RuntimeException like UnsupportedOperationException, so it is unchecked all the same. The default
      ;; methods of java.util.Map would call put, or setValue on an entry, so each throws here first.
      (~'put [~this ~k ~v] (~final-name ~this "put" ~k))
      (~'putAll [~this ~o] (~final-name ~this "putAll" nil))
      (~'remove [~this ~k] (~final-name ~this "remove" ~k))
      (~'remove [~this ~k ~v] (~final-name ~this "remove" ~k))
      (~'clear [~this] (~final-name ~this "clear" nil))
      (~'replaceAll [~this ~f] (~final-name ~this "replaceAll" nil))
      (~'compute [~this ~k ~f] (~final-name ~this "compute" ~k))
      (~'computeIfAbsent [~this ~k ~f] (~final-name ~this "computeIfAbsent" ~k))
      (~'computeIfPresent [~this ~k ~f] (~final-name ~this "computeIfPresent" ~k))
      (~'merge [~this ~k ~v ~f] (~final-name ~this "merge" ~k))
      (~'putIfAbsent [~this ~k ~v] (~final-name ~this "putIfAbsent" ~k))
      (~'replace [~this ~k ~v] (~final-name ~this "replace" ~k))
      (~'replace [~this ~k ~o ~v] (~final-name ~this "replace" ~k))

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
  (when (some #(= "/" (name %)) fields)
    (syntax-error! form ::rule/field-slash
                   (str "a field cannot be named /: the keyword :/ and the Java field it compiles to do not work as a"
                        " field. Rename it, as in slash.")))
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
         ~@(map-type-methods fields this k v `((var final!) ~this "assoc" ~k) `(var final!)
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
         ~@(map-type-methods fields this k v `((var assoc-draft) ~this ~k ~v) `(var fixed!) nil))
       ~(print-method-form name))))

(defn- not-loaded!
  "Throws `::bad-declaration` when `c` is a class a `defparsed` claimed and defined but never registered: an error
  stopped it after its `deftype`s. `where` names what got it. Returns nil for any other class."
  [where ^Class c]
  (when (and (or (.isAssignableFrom DraftValue c) (.isAssignableFrom ParsedValue c))
             (nil? (.get registry c)))
    (when-let [{:keys [ns type role]} (.get owners (.getName c))]
      (throw (ex-info (str where " got " (if (= :draft role) "a draft of " "the type ") (.getName c) ", but the"
                           " defparsed " type " in " ns " failed to load: an error stopped it after it defined its"
                           " classes, so they have no checks. A type is ready only once its defparsed has loaded. Fix"
                           " that error and re-evaluate the defparsed " type ", or reload its namespace, as in"
                           " (require '" ns " :reload). Then build the value again.")
                      {:type ::bad-declaration, :rule ::rule/type-not-loaded, :class (.getName c), :ns ns})))))

;;; ------------------------------------------------ Sums ------------------------------------------------

(defn- parsed-type?
  "Whether `x` is a parsed type [[defparsed]] defined: a class it registered, by identity. A class that only
  implements `ParsedValue` is not."
  [x]
  (and (class? x)
       (.isAssignableFrom ParsedValue ^Class x)
       (identical? x (:parses-to (.get registry x)))))

(defn- member-names
  "The class names of `members`, for comparing and printing sums, or `members` itself when it is not a sequence."
  [members]
  (if (sequential? members) (mapv #(if (class? %) (.getName ^Class %) %) members) members))

;; Not a record, so `assoc` and `merge` cannot build a sum from another. Its constructor can, so every use checks the
;; members again, with [[valid-sum?]].
(defclass-once
  (deftype Sum [name members]
    Object
    (equals [_ o] (and (instance? Sum o)
                       (= name (.-name ^Sum o))
                       (= (member-names members) (member-names (.-members ^Sum o)))))
    (hashCode [_] (hash [name (member-names members)]))
    (toString [_] (str "#<Sum " name " " (let [ns (member-names members)] (if (sequential? ns) (mapv symbol ns) ns))
                       ">"))))

;; only `sum`, which checks the members, builds one
(ns-unmap *ns* '->Sum)

(defmethod print-method Sum [x ^java.io.Writer w]
  (.write w (str x)))

(defn- sum? [x] (instance? Sum x))

(defn- sum-name [^Sum s] (.-name s))

(defn- sum-members [^Sum s] (.-members s))

(defn- valid-members?
  "Whether `members` can be a sum's members: a sequence of distinct parsed types."
  [members]
  (boolean (and (sequential? members)
                (seq members)
                (every? parsed-type? members)
                (apply distinct? (map #(.getName ^Class %) members)))))

(defn- valid-sum?
  "Whether `x` is a sum whose members are distinct parsed types, as [[sum]] builds one."
  [x]
  (and (sum? x) (valid-members? (sum-members x))))

(defn- member?
  "Whether the class `cls` is `type`, or one of the members of the sum `type`: the class itself, as for a record."
  [type cls]
  (if (sum? type) (boolean (some #(identical? % cls) (sum-members type))) (identical? type cls)))

(defn- named-member
  "The class among `type`, or the members of the sum `type`, with the name of the class `cls`, or nil. It is `cls`
  itself unless one of the two is from before a REPL re-evaluated their `defparsed`."
  [type ^Class cls]
  (when (some? cls)
    (some #(when (= (.getName ^Class %) (.getName cls)) %) (if (sum? type) (sum-members type) [type]))))

(defn- stale?
  "Whether the parsed class `c` is not the one its `defparsed` defined last."
  [^Class c]
  (not (identical? c (.get latest (.getName c)))))

(defn- defined-in
  "`the defparsed Debit in bank`, for the parsed class `c`."
  [^Class c]
  (str "the defparsed " (.getSimpleName c) " in " (:ns (.get owners (.getName c)))))

(defn- class-ns
  "The namespace whose `defparsed` claimed the class `c`, or nil."
  [^Class c]
  (:ns (.get owners (.getName c))))

(defn- reload-command
  "`(require 'a 'b :reload)` for the namespaces `nss`, in order, each once: reloading them in that order re-evaluates
  each against the ones before it."
  [nss]
  (str "(require " (clojure.string/join " " (map #(str "'" %) (distinct nss))) " :reload)"))

(defn- stale-value!
  "Throws `::stale-type`: `what`, a value or draft of the parsed class `c`, is from before `c`'s `defparsed` was
  re-evaluated, and the code that got it expects the class defined since."
  [where what ^Class c]
  (throw (ex-info (str where " got " what " of " (.getName c) " from before " (defined-in c) " was re-evaluated. Types"
                       " compare by class, like records, and re-evaluating a defparsed makes new classes. Build it"
                       " again from a new draft, as in (pt/parse (->" (.getSimpleName c) "Draft ...)), or re-run the"
                       " code that built it.")
                  {:type ::stale-type, :rule ::rule/stale-value, :class (.getName c)})))

(defn- type-name [type]
  (if (sum? type) (str (sum-name type)) (.getSimpleName ^Class type)))

(defn- sum
  "A sum of parsed types, named `name`. Use [[defsum]]."
  [name members]
  (when (sequential? members)
    (doseq [m members :when (class? m)] (not-loaded! (str "defsum " name) m)))
  (when-not (valid-members? members)
    (throw (ex-info (str "defsum " name " got "
                         (if (sequential? members)
                           (mapv #(if (class? %) (symbol (.getSimpleName ^Class %)) %) members)
                           (pr-str members))
                         ". A sum's members are distinct types defined with defparsed. List each parsed type once, as"
                         " in (defsum " name " [Debit Credit]).")
                    {:type    ::bad-declaration
                     :rule    ::rule/bad-sum-members
                     :name    name
                     :members (if (sequential? members) (mapv pr-str members) (pr-str members))})))
  (Sum. name (vec members)))

(defmacro defsum
  "Define `name`, a closed set of parsed types. [[match]] on it must handle every member, and a child field of a sum
  type holds drafts that parse to any member.

    (defsum Transaction [Debit Credit])"
  [name members]
  (when-not (simple-symbol? name)
    (throw (ex-info (str "defsum: the name " (pr-str name) " is not a symbol. A sum is a var, named by a symbol. Write"
                         " (defsum Transaction [Debit Credit]), with a symbol for the name.")
                    {:type ::bad-declaration, :rule ::rule/sum-name-not-symbol, :name (pr-str name)})))
  ;; through `var`, because it is private
  `(def ~name ((var sum) '~name ~members)))

(defmacro match
  "Branch on which member of the sum `sum-name` the parsed value `x` is. Each clause is a member type, a binding, and
  an expression:

    (match Transaction transaction
      Debit  debit  (debit! debit)
      Credit credit (credit! credit))

  Leaving out a member, naming a type that is not one, or naming one twice is a compile error. A branch runs only for
  a value [[parse]] issued, of its type's class. A value of a class with the type's name, from before or after a REPL
  re-evaluated its `defparsed`, throws `::stale-type`, naming the namespace to re-evaluate. Any other `x` throws
  `::not-parsed`."
  [sum-name x & clauses]
  (let [refuse! (fn [rule message data]
                  (throw (ex-info message (assoc data :type ::bad-declaration, :rule rule, :sum (pr-str sum-name)))))
        _       (when-not (symbol? sum-name)
                  (refuse! ::rule/match-sum-not-a-symbol
                           (str "match: the sum " (pr-str sum-name) " is not a symbol. match names a sum by its"
                                " symbol. Write "
                                (if (or (string? sum-name) (keyword? sum-name))
                                  (pr-str (list* 'pt/match (symbol (name sum-name)) x clauses))
                                  "(pt/match Transaction x Debit d (f d) Credit c (g c))")
                                ".")
                           {}))
        sum-var (resolve sum-name)
        s       (when (var? sum-var) @sum-var)
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
        members (mapv #(symbol (.getSimpleName ^Class %)) (sum-members s))
        types   (map (fn [[t]] (let [_ (when-not (symbol? t)
                                         (refuse! ::rule/match-not-a-type
                                                  (str "match on " sum-name ": the clause " (pr-str t) " is not a symbol."
                                                       " Each clause starts with a member type, by its symbol. Write "
                                                       (if (or (string? t) (keyword? t)) (symbol (name t)) (first members))
                                                       " for " (pr-str t) ", naming one of " members ".")
                                                  {:clause (pr-str t)}))
                                     c (resolve t)]
                                 (when (class? c) (not-loaded! (str "match on " sum-name) c))
                                 (when-not (class? c)
                                   (refuse! ::rule/match-not-a-type
                                            (str "match on " sum-name ": " t " is not a type. Each clause starts"
                                                 " with a member type. Name one of "
                                                 (mapv #(symbol (.getSimpleName ^Class %)) (sum-members s)) ".")
                                            {:clause (str t)}))
                                 c))
                     triples)
        name-of (fn [^Class c] (.getName c))
        simple  (fn [^Class c] (symbol (.getSimpleName c)))
        missing (remove (fn [m] (some #(identical? m %) types)) (sum-members s))
        extra   (remove (fn [t] (some #(identical? t %) (sum-members s))) types)]
    (when-let [[t] (seq (filter (fn [t] (some #(= (name-of t) (name-of %)) missing)) extra))]
      (let [sum-ns (ns-name (.ns ^clojure.lang.Var sum-var))
            nss    [sum-ns (ns-name *ns*)]]
        (throw (ex-info (str "match on " sum-name ": the clause " (simple t) " names " (name-of t) ", but the sum holds"
                             " another class of that name: one of the two is from before " (defined-in t) " was"
                             " re-evaluated. Types compare by class, like records. "
                             (if (apply = (class-ns t) nss)
                               (str "Re-evaluate the defsum " sum-name " after the defparsed " (simple t) ", then this"
                                    " match, or reload the namespace, as in " (reload-command nss) ".")
                               (str "Reload the namespace of the defsum " sum-name ", then this match's, in that order,"
                                    " as in " (reload-command nss) ".")))
                        {:type ::stale-type, :rule ::rule/stale-sum, :sum (str sum-name), :class (name-of t)
                         :sum-ns sum-ns}))))
    (when (seq missing)
      (let [simple (mapv simple missing)]
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
    ;; distinct by name: two classes of one name are the type before and after a REPL re-evaluated it
    (when-not (apply distinct? (map name-of types))
      (refuse! ::rule/match-member-twice
               (str "match on " sum-name " handles a type twice: "
                    (vec (for [[t c] (frequencies (map #(symbol (.getSimpleName ^Class %)) types)) :when (< 1 c)] t))
                    ". One clause runs per member. Keep one clause per type.")
               {}))
    (let [v (gensym "v")]
      `(let [~v ~x]
         (cond
           ;; the private fns through `var`
           ~@(mapcat (fn [[t b e]]
                       [`((var issued-as?) ~t ~v) `(let [~b ~v] ~e)])
                     triples)
           ;; `v` is no member this match handles, so this throws
           :else ((var match-fell-through!) ~sum-name ~(vec (map first triples)) '~(ns-name *ns*)
                  '~(ns-name (.ns ^clojure.lang.Var sum-var)) ~v))))))

;;; ------------------------------------------------ defparsed ------------------------------------------------

(defn- claim-class-names!
  "Throws `::bad-declaration` unless the `defparsed` of `parsed-name` in the namespace `ns-sym` may define the classes
  in `claims`, a map from class name to its role, `:parsed` or `:draft`: each name is unclaimed, or claimed by this
  namespace in the same role, as when a REPL re-evaluates the form. When `record?`, records the claims."
  [ns-sym parsed-name claims record?]
  (locking owners
    (doseq [[class-name role] claims
            :let  [{owner :ns, owner-role :role, owner-type :type} (.get owners class-name)]
            :when owner]
      (cond
        (not= owner ns-sym)
        (throw (ex-info (str "defparsed " parsed-name ": " class-name " is already defined by a defparsed in the"
                             " namespace " owner ", and the namespace " ns-sym " compiles to the same Java class"
                             " name. One namespace's defparsed would redefine the other's classes. Rename one of the"
                             " namespaces, as in " ns-sym "-2.")
                        {:type ::bad-declaration, :rule ::rule/class-name-taken, :class class-name, :owner owner
                         :ns ns-sym}))

        (not= owner-role role)
        (throw (ex-info (str "defparsed " parsed-name ": " class-name " is already the " (name owner-role) " type of"
                             " (defparsed " owner-type " ...) in " owner ", and this defparsed would make it its "
                             (name role) " type. A class name has one role, and the draft of Name is NameDraft."
                             " Rename one of the types, as in (defparsed " parsed-name "2 ...).")
                        {:type ::bad-declaration, :rule ::rule/class-name-other-role, :class class-name
                         :role role, :owner-role owner-role, :owner-type owner-type}))))
    (when record?
      (doseq [[class-name role] claims]
        (.put owners class-name {:ns ns-sym, :role role, :type parsed-name})))))

(defn- parse-check-clause
  "`(pure name doc [field ...] body ...)` or `(external name doc [field ...] [local fetch ...]? body ...)`, a clause
  of the `defparsed` of `parsed-name`."
  [parsed-name fields fetch-names [head n doc & more :as form]]
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
    ;; Purely syntactic: any symbol in the body equal to a field the argument vector does not name, quoted or bound
    ;; as a local too. An unnamed field is usually an unresolved symbol, but a field named like a var (`count`, or a
    ;; fn of this namespace) would resolve to the var instead, so look for every field by name.
    (when-let [unnamed (seq (distinct (filter (set (remove (set args) fields)) (filter symbol? (tree-seq coll? seq body)))))]
      (let [field (first unnamed)
            argv  (vec (concat args unnamed))
            v     (resolve field)]
        (syntax-error! form ::rule/check-uses-unnamed-field
                       (if (var? v)
                         (str "`" field "` is a field of " parsed-name " and also " (symbol v) ". If you meant the"
                              " field, add it to the argument vector: " argv ". If you meant the function, write "
                              (symbol v) ".")
                         (str what " uses the field `" field "` without naming it. Add it to the argument vector: "
                              argv ". If it is a local, rename the local, as in " field "-value.")))))
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
      (pure ::balanced \"The debits and the credits add up to the same amount.\"
            [lines]
            (= (reduce + 0M (map :amount (filter #(instance? Debit %) lines)))
               (reduce + 0M (map :amount (filter #(instance? Credit %) lines)))))
      (pure ::one-user \"Every line is made by the entry's user.\"
            [user-id lines]
            (every? #(= user-id (:user-id %)) lines)))

  Clauses:

  - `(pure name doc [field ...] body ...)`: a check over the named fields.
  - `(external name doc [field ...] [local fetch-name ...]? body ...)`: a check over the named fields and the
    results of the named fetches.
  - `(fetch name [field ...] f)`: `f` takes every distinct tuple of the fields, and returns a map from each tuple to
    what it fetched.
  - `(children field ParsedType)`: the field holds a vector of drafts that parse to `ParsedType`.
  - `(child field ParsedType)`: the field holds nil or a draft that parses to `ParsedType`.

  A check takes no options. Its argument vector is the fields it reads: one that is not a field is a compile error.

  Pure checks run in the order written, after the shape checks of the child fields. A pure check is held, not run,
  when one before it that shares a field ran and failed. A held check holds nothing. A pure check written before one that reads a proper subset of its
  fields is a compile error: move the smaller one first.

  Write `defparsed` at the top level of a namespace: inside another form it throws `::bad-declaration`.

  The plan [[parse]] follows is built and checked when the form loads, so every declaration error throws
  `::bad-declaration` then, not at the first parse. The fetch fns and the child types are evaluated and checked
  before any class is defined, so a re-evaluation they refuse leaves the type as it was. A fetch fn therefore can't
  name the type being defined; a child field may, by its own name. Only `defparsed` defines parsed types: the draft type and the
  parsed type are defined together, so the draft type is the only one that parses to its parsed type.

  The parsed type has no `->Name` factory: only [[parse]] builds a value of it that [[parsed]] accepts."
  [parsed-name fields & clauses]
  ;; Inside another form, the whole form compiles before any of it runs, so the classes would be defined before the
  ;; checks below them run, and their names would not resolve. `&env` holds the locals, and `Compiler/METHOD` is bound
  ;; while a fn body compiles, which a `when` at the top level is.
  (when (or (seq &env) (some? (.deref clojure.lang.Compiler/METHOD)))
    (syntax-error! &form ::rule/defparsed-not-top-level
                   (str "(defparsed " (pr-str parsed-name) " ...) is inside another form. defparsed defines classes,"
                        " then checks and registers them, one step at a time, which works only at the top level of a"
                        " namespace. Write defparsed at the top level of a namespace, and move a local a check or"
                        " fetch uses into a def, as in (def limit 10).")))
  (when (and (simple-symbol? parsed-name) (clojure.string/includes? (name parsed-name) "."))
    (syntax-error! &form ::rule/name-with-dot
                   (str "the name " parsed-name " has a dot. A parsed type is a class in this namespace's package, and"
                        " a dot would put it in another package, which Clojure cannot load it from. Name it without"
                        " one, as in " (clojure.string/replace (name parsed-name) "." "") ".")))
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
        checks      (mapv #(parse-check-clause parsed-name fields fetch-names %)
                          (filter (comp '#{pure external} first) clauses))
        children    (into {}
                          (for [[head field type :as c] (concat (get by-head 'child) (get by-head 'children))]
                            (do (when-not (contains? (set fields) field)
                                  (syntax-error! c ::rule/child-not-a-field
                                                 (str "the child field " (pr-str field) " is not a field of the"
                                                      " draft, and a child field must be a field of the draft."
                                                      (did-you-mean field fields) " Name one of " fields ".")))
                                [(keyword field) {:kind    (if (= 'child head) :one :many)
                                                  :type    type
                                                  ;; for a stale-type message: where to reload the child type
                                                  :type-ns (let [r (when (symbol? type)
                                                                     (if (= type parsed-name)
                                                                       *ns*
                                                                       (try (resolve type) (catch Exception _ nil))))]
                                                             (cond
                                                               (instance? clojure.lang.Namespace r) (ns-name r)
                                                               (var? r)   (ns-name (.ns ^clojure.lang.Var r))
                                                               (class? r) (class-ns r)))}])))
        _           (doseq [{n :name, :keys [reads]} fetches
                            :let [kids (filter (set (keys children)) reads)]
                            :when (seq kids)]
                      (syntax-error! &form ::rule/fetch-reads-child-field
                                     (str "the fetch " n " reads the child field " (symbol (first kids)) ". A fetch"
                                          " runs before the children's external checks, so it would read children"
                                          " that may fail them. Read the children in the check itself, as in"
                                          " (external ::check \"doc\" [" (symbol (first kids)) "] ...), and fetch"
                                          " by the parent's own fields.")))
        child-heads (map second (concat (get by-head 'child) (get by-head 'children)))
        _           (when-not (apply distinct? ::sentinel child-heads)
                      (syntax-error! &form ::rule/child-declared-twice
                                     (str "a field is declared as a child twice: "
                                          (vec (for [[f c] (frequencies child-heads) :when (< 1 c)] f))
                                          ". Declare it once, with child or children.")))
        names       (concat (map :name checks) (map :name fetches))
        draft-name  (symbol (str parsed-name "Draft"))
        parsed-class-name (str (namespace-munge *ns*) "." parsed-name)
        claims      {parsed-class-name                          :parsed
                     (str (namespace-munge *ns*) "." draft-name) :draft}]
    (when-not (apply distinct? ::sentinel names)
      (syntax-error! &form ::rule/name-used-twice
                     (str "a check or fetch name is used twice: "
                          (vec (for [[x c] (frequencies names) :when (< 1 c)] x))
                          ". A failure is reported under its name. Give each its own name.")))
    (when-let [[big small] (first (for [[i big] (map-indexed vector (filter #(= :pure (:kind %)) checks))
                                        small (drop (inc i) (filter #(= :pure (:kind %)) checks))
                                        :let  [b (set (:reads big)), sm (set (:reads small))]
                                        ;; a check that reads no field shares none, so it holds nothing
                                        :when (and (seq sm) (set/subset? sm b) (not= sm b))]
                                    [big small]))]
      (syntax-error! &form ::rule/pure-check-order
                     (str "the pure check " (:name big) " reads " (mapv symbol (:reads big)) ", and is written before "
                          (:name small) ", which reads only " (mapv symbol (:reads small)) ". Pure checks run in the"
                          " order written, and a check that fails holds every later one that shares a field with it, so a check"
                          " on fewer fields goes first. Move " (:name small) " before " (:name big) ".")))
    (when-let [reserved (seq (filter #(= "parsed-types.child" (namespace %)) names))]
      (syntax-error! &form ::rule/reserved-name
                     (str "names in parsed-types.child are reserved for child-field checks: " (vec reserved)
                          ". Name them in your own namespace, as in ::" (name (first reserved)) ".")))
    ;; when it expands, so that a refused defparsed never reaches its deftypes, which would redefine the classes
    (claim-class-names! (ns-name *ns*) parsed-name claims false)
    (let [self?       #(contains? #{parsed-name (symbol parsed-class-name)} %)
          child-types (into {} (for [[field {:keys [type]}] children :when (not (self? type))] [field type]))]
      ;; A top-level `do` runs each form before it compiles the next, so a check that fails in the first form leaves
      ;; every class as it was. The private fns through `var`.
      `(do
         ;; the fetch fns and the child types, evaluated once and checked before any class is defined
         ((var stage!) ~parsed-class-name ~(str (namespace-munge *ns*) "." draft-name)
                       '~(mapv #(select-keys % [:name :reads]) fetches) [~@(map :f fetches)]
                       '~(update-vals children #(select-keys % [:kind])) ~child-types)
         ;; before the deftypes, which is what records the claim
         ((var claim-class-names!) '~(ns-name *ns*) '~parsed-name ~claims true)
         ~(defparsed-type parsed-name fields)
         ~(defdraft-type draft-name fields)
         ;; only this expansion may record a plan
         ((var register!) ~parsed-name ~draft-name
                          {:checks   ~checks
                           :fetches  '~(mapv #(select-keys % [:name :reads]) fetches)
                           :children '~(update-vals children #(dissoc % :type))}
                          ~(builders parsed-name draft-name fields))
         ~parsed-name))))

;;; ------------------------------------------------ Plans ------------------------------------------------

(declare plan-for)

(defn- bad-declaration!
  "Throws `::bad-declaration` for the declaration of the draft class named `draft-class-name`, which broke `rule`, when
  its `defparsed` loads."
  [^String draft-class-name rule message data]
  (throw (ex-info (str (subs draft-class-name (inc (.lastIndexOf draft-class-name "."))) ": " message)
                  (merge {:type ::bad-declaration, :rule rule, :draft draft-class-name} data))))

(defn- draft-value?
  "Whether `x` is a draft of a type [[defparsed]] defined. A type that only implements `DraftValue` is not."
  [x]
  (and (instance? DraftValue x) (some? (.get registry (class x)))))

(defn- stage!
  "Check what [[defparsed]] cannot see when it expands, only when it loads, before it defines any class: a child type
  that is not a parsed type or a sum, and a fetch that is not a fn. Throws `::bad-declaration`, so a `defparsed` that
  fails here changes nothing. Then keep `fetch-fns` and `child-types` for [[register!]]. `fetches` holds each
  fetch's `:name` and `:reads`, in the order of `fetch-fns`; `children` maps each child field to its `:kind`, and
  `child-types` maps each child field but those of the type being defined to its type. Called by the expansion of
  [[defparsed]]."
  [parsed-class-name draft-class-name fetches fetch-fns children child-types]
  (doseq [[field type] child-types
          :let [{:keys [kind]} (children field)]]
    (when-not (or (parsed-type? type) (valid-sum? type))
      (bad-declaration! draft-class-name ::rule/child-type-not-parsed
                        (str "the child field " field " holds " (pr-str type) ", which is not a type defined with"
                             " defparsed or a sum defined with defsum, in (" (if (= :one kind) "child" "children") " "
                             (name field) " " (pr-str type) "). A child field holds drafts the parent parses. Name"
                             " one of those.")
                        {:field field, :got (pr-str type)})))
  (doseq [[{n :name, :keys [reads]} f] (map vector fetches fetch-fns)]
    (when-not (fn? f)
      (bad-declaration! draft-class-name ::rule/fetch-not-fn
                        (str "the fetch " n " is " (pr-str f) ", not a fn, in (fetch " n " " (mapv symbol reads) " "
                             (pr-str f) "). A fetch is called with the tuples it fetches. Write a fn of the tuples"
                             " it fetches, as in (fetch " n " " (mapv symbol reads) " rows-by-id).")
                        {:name n, :got (some-> f class .getName)})))
  (.put pending parsed-class-name {:fetch-fns fetch-fns, :child-types child-types})
  nil)

(defn- stale-child!
  "Throws `::stale-type`: the child field `field` of the parent `parent`, a plan or `{:parses-to :draft-class}`,
  declared as `child`, `{:type :type-ns}`, holds the draft `d`, which parses to `c`, a class with the name of the
  type or of one of its members but not that class."
  [{:keys [parses-to draft-class]} field {:keys [type type-ns]} d ^Class c]
  (let [where (str (.getSimpleName ^Class draft-class) ": the child field " field)]
    (if (stale? c)
      (stale-value! where (str "a " (.getSimpleName (class d))) c)
      (let [parent    (.getSimpleName ^Class parses-to)
            parent-ns (class-ns parses-to)
            ;; a sum is re-evaluated in its own namespace first, so it takes the new member
            nss       (if (sum? type) [type-ns parent-ns] [parent-ns])
            ;; re-evaluating a form in place works only when no namespace imports the class from another
            in-place? (apply = (class-ns c) nss)]
        (throw (ex-info (str where " holds a " (.getSimpleName (class d)) " of " (.getName c) ", but "
                             (defined-in parses-to) " was evaluated against an older " (.getName c)
                             (when (sum? type) (str ", through the sum " (sum-name type))) ". Types compare by class,"
                             " like records, so a parent keeps the child types it was evaluated with. "
                             (cond
                               in-place?
                               (str "Re-evaluate " (when (sum? type) (str "the defsum " (sum-name type) ", then "))
                                    "the defparsed " parent ", or reload its namespace, as in " (reload-command nss)
                                    ".")

                               (sum? type)
                               (str "Reload the namespace of the defsum " (sum-name type) ", then the parent's, in that"
                                    " order, as in " (reload-command nss) ".")

                               :else
                               (str "Reload its namespace, which imports the new class, as in " (reload-command nss)
                                    ".")))
                        {:type ::stale-type, :rule ::rule/stale-child-type, :class (.getName c), :field field
                         :parent (.getName ^Class parses-to)}))))))

(defn- children-of-type?
  "Whether `v` is what the child field `field` of `parent`, declared as `{:kind :type}`, may hold: drafts that parse to
  the type's class itself, or one of its members. A draft of a class with that name but not the class throws
  `::stale-type`."
  [parent field {:keys [kind], parsed-type :type, :as child} v]
  (let [drafts-it? (fn [d] (when (instance? DraftValue d)
                                (not-loaded! (str (.getSimpleName ^Class (:draft-class parent)) ": the child field " field)
                                             (class d)))
                              (and (draft-value? d)
                                (let [c (:parses-to (plan-for "pt/parse" d))]
                                  (when (and (named-member parsed-type c) (not (member? parsed-type c)))
                                    (stale-child! parent field child d c))
                                  (member? parsed-type c))))]
    (case kind
      :one  (or (nil? v) (drafts-it? v))
      :many (and (vector? v) (every? drafts-it? v)))))

(defn- child-shape-check
  "The pure check every child field gets: it holds drafts of the declared type. Every check that reads the field waits
  on it."
  [parent field {:keys [kind type], :as child}]
  {:name  (keyword "parsed-types.child" (name field))
   :doc   (str (name field) " holds " (if (= :one kind) "nil or a draft that parses to " "a vector of drafts that parse to ")
               (type-name type) ".")
   :kind  :pure
   :shape? true
   :reads [field]
   :f     (fn [v] (children-of-type? parent field child v))})

(defn- reads-set [x] (set (:reads x)))

(defn- compile-plan
  "Order the checks, and work out what each waits on.

  - A pure check waits on every pure check before it that shares a field with it, and is held when one of those ran
    and failed: one held in turn holds nothing. The shape checks of the child fields come first, so a check that reads a child field waits on its shape. It also waits on the children's own
    pure checks, through [[failed-children]].
  - A fetch waits on every pure check, so malformed input never reads the world.
  - An external check waits on its fetches and on every pure check.

  Pure checks run first, in the order written, then fetches, then external checks, each in the order written.
  [[defparsed]] refuses a pure check written before one that reads a proper subset of its fields, so a check on one
  field runs before a check relating it to another."
  [parses-to draft-class fields {:keys [checks fetches children], :as decl}]
  (let [shape     (into {} (map (fn [[field child]] [field (child-shape-check {:parses-to parses-to, :draft-class draft-class} field child)])) children)
        pure      (into (vec (vals shape)) (filter #(= :pure (:kind %))) checks)
        external  (filterv #(= :external (:kind %)) checks)
        waits-on  (fn [pred] (into #{} (comp (filter pred) (map :name)) pure))
        all-pure  (waits-on any?)
        pure'     (map-indexed (fn [i c]
                                 (assoc c :prereqs (into #{}
                                                         (comp (take i)
                                                               (filter #(seq (set/intersection (reads-set %)
                                                                                               (reads-set c))))
                                                               (map :name))
                                                         pure)))
                               pure)
        fetches'  (for [fd fetches]
                    (assoc fd :prereqs all-pure))
        external' (for [c external]
                    (assoc c :prereqs (into all-pure (:fetches c))))]
    {:draft-class     draft-class
     :parses-to       parses-to
     :declaration     (assoc decl :parses-to parses-to)
     :fields          fields
     :children        children
     :pure            (vec pure')
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
  "Build the plan of `draft-type`, which parses to `parsed-type`, and record it for both, with the `builders`
  [[defparsed]] generated and the fetch fns and child types [[stage!]] kept. Called by the expansion of
  [[defparsed]], when it loads, after its `deftype`s."
  [parsed-type draft-type decl builders]
  (let [{:keys [fetch-fns child-types]} (.remove pending (.getName ^Class parsed-type))
        decl (-> decl
                 (update :fetches #(mapv (fn [fd f] (assoc fd :f f)) % fetch-fns))
                 (update :children #(into {} (map (fn [[field child]]
                                                    [field (assoc child :type (get child-types field parsed-type))]))
                                          %)))
        plan (merge (compile-plan parsed-type draft-type (:fields builders) decl) builders)]
    (.put registry draft-type plan)
    (.put registry parsed-type plan)
    (.put latest (.getName ^Class parsed-type) parsed-type)))

(declare issued?)

(defn- plan-for
  "The plan of `draft`'s type. Throws `::bad-input` for anything but a draft of a type [[defparsed]] defined, naming
  `fn-name`, the fn that got it, and its fix as a call of that fn."
  [fn-name draft]
  (when-not (draft-value? draft)
    (when (instance? DraftValue draft) (not-loaded! fn-name (class draft)))
    (let [call (fn [arg] (if (= "pt/parse-all" fn-name) (str "(pt/parse-all [" arg "])") (str "(" fn-name " " arg ")")))]
      (throw (ex-info (str fn-name " got " (describe draft) ". It takes only drafts of types defparsed defined."
                           (cond
                             (issued? draft)
                             (if (#{"pt/parse" "pt/parse-all"} fn-name)
                               " It is already parsed: pass it as it is, or call (pt/draft-of x) to get its draft."
                               (str " It is already parsed: pass its draft, as in " (call "(pt/draft-of x)") "."))

                             (instance? ParsedValue draft)
                             (str " It is a " (.getSimpleName (class draft)) " that pt/parse did not issue: a value"
                                  " with no proof. Build a draft and pass that, as in "
                                  (call (str "(->" (.getSimpleName (class draft)) "Draft ...)")) ".")

                             (map? draft)
                             (str " Build a draft from its fields, naming every field, as in "
                                  (call "(pt/draft Debit {:user-id u, ...})") ", or with " (call "(->DebitDraft ...)")
                                  ".")

                             :else
                             (str " Build one with its constructor, as in " (call "(->DebitDraft ...)")
                                  ", or with pt/draft.")))
                      {:type ::bad-input, :rule ::rule/not-a-draft, :fn fn-name, :got (some-> draft class .getName)}))))
  (.get registry (class draft)))

(defn declaration
  "The declaration `defparsed` wrote for `draft`'s type: `{:parses-to :checks :fetches :children}`, as data. Throws
  `::bad-input` for anything but a draft."
  [draft]
  (:declaration (plan-for "pt/declaration" draft)))

;;; ------------------------------------------------ Running ------------------------------------------------

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

(def ^:private clock-classes
  "The `java.time` classes that read the time each time they are asked: `Clock`, and `InstantSource` on a JDK that has
  it. Their subclasses are final classes of the `java.time` package, so [[java-time-value?]] alone would accept them."
  (into [java.time.Clock]
        (keep #(try (Class/forName %) (catch ClassNotFoundException _ nil)))
        ["java.time.InstantSource"]))

(defn- children-or-refusal
  "What the walk in [[not-immutable]] does with `v`, at `path`: `[:refuse rule what fix]`, or `[:walk children]`, the
  values inside it to walk, each as `[value path]`, its metadata first."
  [v path]
  (let [c        (class v)
        refuse   (fn [what fix] [:refuse ::rule/mutable-value what fix])
        walk     (fn [items] [:walk (cond->> items
                                      (and (instance? clojure.lang.IObj v) (some? (meta v)))
                                      (cons [(meta v) (conj path '<meta>)]))])]
    (cond
      (contains? immutable-classes c) (walk nil)
      (class? v)                      (walk nil)

      (some #(.isInstance ^Class % v) clock-classes)
      (refuse "a clock, which reads the time each time it is asked" "Pass the instant it reads, as in (.instant clock).")

      (java-time-value? c) (walk nil)
      (issued? v)          (walk nil)

      (and (instance? clojure.lang.Sorted v)
           (not (identical? clojure.lang.RT/DEFAULT_COMPARATOR (.comparator ^clojure.lang.Sorted v))))
      (refuse "a sorted collection with a custom comparator, which can read state that changes"
              "Use (sorted-map ...) or (sorted-set ...), which sort with compare, or a plain map or set.")

      (instance? clojure.lang.IPersistentMap v)
      (walk (mapcat (fn [[k x]] [[k (conj path '<key>)] [x (conj path k)]]) v))

      (or (instance? clojure.lang.IPersistentVector v) (list? v) (instance? clojure.lang.PersistentQueue v))
      (walk (map-indexed (fn [i x] [x (conj path i)]) v))

      (instance? clojure.lang.IPersistentSet v)
      (walk (map (fn [x] [x (conj path '<element>)]) v))

      ;; not (vec a): vec of an Object[] of 32 or fewer elements shares the array
      (.isArray c)                        (refuse "a Java array" "Copy it into a vector, as in (into [] a).")
      (instance? clojure.lang.IAtom v)    (refuse "an atom" "Deref it, as in @a.")
      (instance? clojure.lang.IRef v)     (refuse "a reference type" "Deref it, as in @r.")
      (instance? clojure.lang.Volatile v) (refuse "a volatile" "Deref it, as in @v.")
      ;; java.sql.Date and java.sql.Time throw on .toInstant
      (instance? java.sql.Date v)         (refuse "a mutable date" "Use java.time.LocalDate, as in (.toLocalDate d).")
      (instance? java.sql.Time v)         (refuse "a mutable time" "Use java.time.LocalTime, as in (.toLocalTime d).")
      (instance? java.util.Date v)        (refuse "a mutable date" "Use java.time.Instant, as in (.toInstant d).")

      (instance? clojure.lang.LazySeq v)
      (refuse "a lazy seq, which realizing could run side effects or never end" "Pass a vector, as in (vec s).")

      (instance? clojure.lang.ISeq v) (refuse "a seq, which may be lazy" "Pass a vector, as in (vec s).")

      (or (instance? java.util.Collection v) (instance? java.util.Map v))
      (refuse "a mutable Java collection" "Copy it into a Clojure collection, as in (vec c) or (into {} m).")

      :else
      (refuse "a value not known to be immutable"
              (str "Convert it to an immutable value, such as a string, a number, a java.time value or a persistent"
                   " collection.")))))

(defn- not-immutable
  "nil when `v` is known to be immutable all the way down, else `[rule path what fix class-name]`: the rule, where
  inside the field the first value that is not is, what it is, the fix, and its class. Accepts nil, the values of
  [[immutable-classes]], classes, `java.time` values but clocks, values [[parse]] issued, and persistent vectors, maps
  (records and drafts included), sets, lists and queues whose contents pass, map keys and metadata included. A sorted
  collection must use the default comparator, since a custom one can read state that changes. Refuses everything
  else, lazy and other seqs too: realizing one could run side effects or never end. Refuses a value nested deeper
  than [[max-value-depth]], with the rule `value-too-deep`.

  The walk keeps its own stack, so its depth does not depend on the thread's. `heights` maps each value with values
  inside it that passed in this parse, by identity, to its height: the most levels below it. One reached again is
  skipped when its deepest value is still within the limit from there, so shared structure costs its size, not the
  number of paths to it, and the depth limit holds on every path."
  [v ^java.util.IdentityHashMap heights]
  ;; A frame is [:visit value path parent] or [:exit value acc parent], where `acc` and `parent` are 1-element arrays
  ;; that collect the height of a value from its children: each child's height plus one.
  (let [bump! (fn [^longs parent ^long h] (when parent (aset parent 0 (max (aget parent 0) (inc h)))))]
    (loop [stack (list [:visit v [] nil])]
      (when-let [[op x a parent] (first stack)]
        (let [stack (rest stack)]
          (if (= :exit op)
            (let [h (aget ^longs a 0)]
              (.put heights x h)
              (bump! parent h)
              (recur stack))
            (let [path  a
                  depth (count path)
                  known (when (some? x) (.get heights x))]
              (cond
                (< max-value-depth depth)
                [::rule/value-too-deep path (str "a value nested deeper than " max-value-depth " levels")
                 (str "Store a flatter value: for nested vectors, (vec (flatten v)), or keep it as text, as in"
                      " (pr-str v).")
                 (.getName (class x))]

                (nil? x)
                (do (bump! parent 0) (recur stack))

                (and known (<= (+ depth (long known)) max-value-depth))
                (do (bump! parent known) (recur stack))

                :else
                (let [[result & more] (children-or-refusal x path)]
                  (if (= :refuse result)
                    (let [[rule what fix] more] [rule path what fix (.getName (class x))])
                    (let [children (first more)]
                      (if (empty? children)
                        (do (bump! parent 0) (recur stack))
                        (let [acc (long-array 1)]
                          (recur (into (conj stack [:exit x acc parent])
                                       (map (fn [[cv cp]] [:visit cv cp acc]))
                                       (reverse children))))))))))))))))

(defn- refuse-mutable!
  "Throws `::bad-input` when a field of `draft` holds, anywhere inside it, a value that is not known to be immutable:
  the checks would prove a value the parsed value does not keep. Child fields are not walked, because each child is
  checked as its own node. There is no opt-out. `walked` is as for [[not-immutable]]."
  [draft {:keys [children]} values walked]
  (doseq [[field v] values
          ;; only a vector, the shape a `children` field takes: a list the reader read carries its line, and fails the
          ;; shape check instead. A draft, the shape a `child` field takes, carries no metadata.
          :when       (and (= :many (get-in children [field :kind])) (vector? v) (some? (meta v)))]
    (throw (ex-info (str (.getSimpleName (class draft)) ": the child field " field " holds a vector with the metadata "
                         (pr-str (meta v)) ". The parsed value holds a new vector of the parsed children, so the"
                         " metadata would be dropped. Keep the metadata beside the draft, as in {:value x, :meta m},"
                         " and pass the vector without it, as in (with-meta v nil).")
                    {:type ::bad-input, :rule ::rule/child-field-metadata, :draft (.getName (class draft))
                     :field field})))
  (doseq [[field v] values
          :when       (not (contains? children field))
          :let        [[rule path what fix got] (not-immutable v walked)]
          :when       what]
    (let [shown (if (< 8 (count path)) (str (subvec path 0 8) " and " (- (count path) 8) " levels more") path)]
      (throw (ex-info (str (.getSimpleName (class draft)) ": the field " field
                           (when (seq path) (str " at " shown))
                           " holds " what " (" got "). A parsed value holds only values known to be immutable, so"
                           " what its checks proved stays true"
                           (when (= rule ::rule/value-too-deep) ", and proving it walks a bounded depth")
                           ". " fix)
                      (merge {:type ::bad-input, :rule rule, :draft (.getName (class draft)), :field field
                              :path path, :got got}
                             ;; a path over the limit is long: the ex-data keeps its first steps and its length
                             (when (= rule ::rule/value-too-deep)
                               {:path (subvec path 0 8), :depth (count path)})))))))

(defn- start [fn-name draft walked]
  (let [plan   (plan-for fn-name draft)
        ;; every field read once, so the checks and the parsed value see the same values
        values ((:read-draft plan) draft)]
    (refuse-mutable! draft plan values walked)
    {:draft   draft
     :values  values
     :plan    plan
     :fetched {}
     :failed  {}     ; check name -> failure entry
     :order   []}))  ; failed check names, in the order they failed

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
  "The drafts of the tree under `root`, children before parents, each with its `:path` from the root. `fn-name` names
  the fn that got `root`, and `walked` is as for [[not-immutable]]."
  [fn-name tree root walked]
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
            st   (assoc (start fn-name draft walked) :tree tree :path path)
            ;; the child fields of the right shape: their children are walked, and an empty vector is rebuilt too
            ok   (filterv (fn [[field child]] (children-of-type? (:plan st) field child (get-in st [:values field])))
                          (get-in st [:plan :children]))
            kids (for [[field {:keys [kind]}] ok
                       :let  [v (get-in st [:values field])]
                       [i d] (if (= :one kind) (when v [[nil v]]) (map-indexed vector v))]
                   [field (child-path path field i) d])
            st   (assoc st :child-paths (reduce (fn [m [field p]] (update m field conj p))
                                                (into {} (map (fn [[field]] [field []])) ok)
                                                kids))]
        ;; pushing the children in order pops the last first, so the reverse of this pre-order is a post-order with
        ;; the children in order
        (recur (into (rest stack) (map (fn [[_ p d]] [d p (inc depth)])) kids) (conj pre st)))
      (vec (rseq pre)))))

(defn- roots-of
  "The checks among `prereqs` that ran and failed, or nil. A held check did not run, so it holds nothing."
  [st prereqs]
  (seq (filter #(contains? (get-in st [:failed %]) :returned) prereqs)))

(defn- block
  "`st` with the check `n` listed under `:blocked` of each of `roots`, the checks that held it."
  [st n roots]
  (reduce #(update-in %1 [:failed %2 :blocked] (fnil conj []) n) st roots))

(defn- run-check [st {n :name, :keys [prereqs f doc reads fetches], :as c}]
  (if-let [roots (roots-of st prereqs)]
    (block st n roots)
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
  "The node's field values, with each child field holding `(built path)` for the path of each of its children: a new
  vector for `children`, empty ones included, and nil or the one child for `child`."
  [{:keys [plan values child-paths]} built]
  (reduce-kv (fn [m field paths]
               (assoc m field (if (= :one (get-in plan [:children field :kind]))
                                (some-> (first paths) built)
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
  theirs. In a tree that failed a pure check, none runs: each is only listed under the pure checks of its node that
  failed."
  [nodes]
  (first (reduce (fn [[out bad] {:keys [tree path child-paths world?], :as st}]
                   (let [externals (get-in st [:plan :external])
                         st        (if world?
                                     (run-checks st externals bad)
                                     (reduce (fn [st {n :name, :keys [prereqs]}]
                                               (if-let [roots (roots-of st prereqs)] (block st n roots) st))
                                             st externals))
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
  "Run the checks of every draft, and of every draft under it, and return one vector of nodes per draft. `fn-name`
  names the fn that got them."
  [fn-name drafts]
  (let [walked       (java.util.IdentityHashMap.)
        nodes        (pure-phase (into [] (comp (map-indexed (fn [i d] (flatten-tree fn-name i d walked))) cat) drafts))
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

(defn- assoc-draft
  "`draft` with `field` set to `v`: a new draft. This is what `assoc` on a draft does; call `assoc`."
  [draft field v]
  (let [{:keys [fields make-draft read-draft]} (plan-for "assoc" draft)]
    (when-not (some #{field} fields)
      (no-field! draft field "assoc"))
    (make-draft (assoc (read-draft draft) field v))))

(defn parse-all
  "Parse each draft and the drafts under it. Returns a vector of parsed values aligned with `drafts`, or throws
  `::invalid` once with every failure: `:failures` maps the index of each draft that failed to its failures, shaped
  like [[parse]]'s, and `:failed-kinds` holds the kinds of check that failed in any of them. Exceptions out of a check
  or fetch propagate, so an external check that throws stops the call. Throws `::bad-input` when `drafts` is a single
  draft, a map, or not a sequential collection."
  [drafts]
  (when (or (map? drafts) (not (sequential? drafts)))
    (throw (ex-info (str "pt/parse-all got " (describe drafts) ". It takes a vector of drafts and returns a vector of"
                         " parsed values. Pass a vector, as in (pt/parse-all [draft]), or parse one draft with"
                         " (pt/parse draft).")
                    {:type ::bad-input, :rule ::rule/parse-all-not-a-vector, :got (some-> drafts class .getName)})))
  (let [trees    (run-all "pt/parse-all" drafts)
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
  (let [nodes (first (run-all "pt/parse" [draft]))]
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

(defn- issued-as?
  "Whether `x` is a value [[parse]] issued, of the class `c` itself. Called by the expansion of [[match]]."
  [c x]
  (and (identical? c (class x)) (issued? x)))

(defn- stale-sink!
  "Throws `::stale-type` when `x`, a value [[parse]] issued, is of a class with the name of `type`, or of a member of
  the sum `type`, but not that class: `fn-name`, a sink's call, and the value disagree on which class is the type."
  [fn-name type x]
  (when (issued? x)
    (let [c (class x), expected (named-member type c)]
      (when (and expected (not (identical? expected c)))
        (if (stale? c)
          (stale-value! fn-name (str "a parsed " (.getSimpleName ^Class c)) c)
          (throw (ex-info (str fn-name " got a parsed " (.getName ^Class c) ", but its type argument " (type-name type)
                               " holds an older class of that name: the code calling (" fn-name " " (type-name type)
                               " ...) was compiled before " (defined-in c) " was re-evaluated. Types compare by class,"
                               " like records, so code keeps the classes it was compiled against. Re-evaluate "
                               (when (sum? type) (str "the defsum " (type-name type) ", then "))
                               "the namespace that calls " fn-name ", or run (clojure.tools.namespace.repl/refresh).")
                          {:type ::stale-type, :rule ::rule/stale-sink, :class (.getName ^Class c)})))))))

(defn- check-type!
  "Throws `::bad-input` unless `t`, the first argument `fn-name` got, is a type [[defparsed]] defined or a sum
  [[defsum]] defined."
  [fn-name t]
  (when-not (or (parsed-type? t) (valid-sum? t))
    (when (class? t) (not-loaded! fn-name t))
    (let [draft-of-type (when (and (class? t) (.isAssignableFrom DraftValue ^Class t))
                          (some-> ^Class (:parses-to (.get registry t)) .getSimpleName symbol))]
      (throw (ex-info (str fn-name " got " (if (class? t) (.getSimpleName ^Class t) (pr-str t)) " as its type. Its"
                           " first argument is a type defined with defparsed, or a sum defined with defsum."
                           (if draft-of-type
                             (str " That is a draft type: pass its parsed type, as in (" fn-name " " draft-of-type
                                  " x).")
                             (str " Pass one, as in (" fn-name " Debit x).")))
                      {:type ::bad-input, :rule ::rule/not-a-parsed-type, :fn fn-name
                       :got  (if (class? t) (.getName ^Class t) (pr-str t))})))))

(defn parsed?
  "Whether `x` is a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`: a value of
  the class itself, as for a record, whose hidden proof field holds the proof only `parse` sets. Throws `::bad-input`
  when `parsed-type` is neither. Throws `::stale-type` when `x` is a value `parse` issued of a class with the name of
  the type but not the class: one of the two is from before a REPL re-evaluated its `defparsed`, and the message
  says which to re-evaluate."
  [parsed-type x]
  (check-type! "pt/parsed?" parsed-type)
  (stale-sink! "pt/parsed?" parsed-type x)
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
      (clojure.string/join " or " (map one (sum-members type)))
      (one type))))

(defn parsed
  "`x`, which must be a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`. Throws
  `::not-parsed` otherwise, and `::stale-type` as [[parsed?]] says. Read its fields by keyword, as in `(:amount x)`: a
  key that is not a field throws `::no-field`. Throws `::bad-input` when `parsed-type` is not a parsed type or a sum."
  [parsed-type x]
  (check-type! "pt/parsed" parsed-type)
  (stale-sink! "pt/parsed" parsed-type x)
  (when-not (parsed? parsed-type x)
    (let [t                      (type-name parsed-type)
          {:keys [rule message]} (cond
                                   (nil? x)
                                   {:rule    ::rule/got-nil
                                    :message (str "Expected a parsed " t ", got nil. A sink takes only a value"
                                                  " pt/parse issued. Write " (parse-hint parsed-type)
                                                  " and pass the result.")}

                                   (and (draft-value? x) (named-member parsed-type (:parses-to (plan-for "pt/parsed" x))))
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
                                                  " value pt/parse issued. Build a draft from its fields and parse"
                                                  " it, as in " (parse-hint parsed-type) ", and pass the result.")}

                                   :else
                                   {:rule    ::rule/got-other-type
                                    :message (str "Expected a parsed " t ", got " (describe x) ". A sink takes only"
                                                  " a value of its type. Pass a " t " instead: "
                                                  (parse-hint parsed-type) ".")})]
      (throw (ex-info message
                      {:type     ::not-parsed
                       :rule     rule
                       :expected (if (sum? parsed-type) (str (sum-name parsed-type)) (.getName ^Class parsed-type))
                       :got      (some-> x class .getName)}))))
  x)

(defn- match-fell-through!
  "Throws for `v`, which no clause of a [[match]] on `sum` took. `handled` holds the classes the match handles,
  `ns-sym` names the namespace it was compiled in, and `sum-ns` the namespace of the defsum. `::stale-type` when `v`
  was issued with a class of the name of one it handles, `::not-parsed` otherwise. Called by the expansion of
  [[match]]."
  [sum handled ns-sym sum-ns v]
  (when (issued? v)
    (let [c (class v)]
      (when (some #(= (.getName ^Class %) (.getName c)) handled)
        (if (stale? c)
          (stale-value! (str "match on " (sum-name sum)) (str "a parsed " (.getSimpleName c)) c)
          (throw (ex-info (str "match on " (sum-name sum) ", compiled in the namespace " ns-sym ", got a parsed "
                               (.getName c) ", but its clause " (.getSimpleName c) " names an older class of that"
                               " name: the match was compiled before " (defined-in c) " was re-evaluated. Types"
                               " compare by class, like records, so a match keeps the classes it was compiled"
                               " against. "
                               (if (= sum-ns ns-sym)
                                 (str "Re-evaluate the namespace " ns-sym ", as in " (reload-command [ns-sym]) ".")
                                 (str "Reload the namespace of the defsum " (sum-name sum) ", then the match's, in that"
                                      " order, as in " (reload-command [sum-ns ns-sym]) ".")))
                          {:type ::stale-type, :rule ::rule/stale-match, :class (.getName c), :ns ns-sym
                           :sum-ns sum-ns}))))))
  (when (and (some? v) (member? sum (class v)) (issued? v))
    (let [got     (symbol (.getSimpleName (class v)))
          handled (mapv #(symbol (.getSimpleName ^Class %)) handled)]
      (throw (ex-info (str "match on " (sum-name sum) " got a parsed " got ", but this match was compiled before " got
                           " joined the sum, and handles only " handled ". A match handles every member of its sum."
                           " Re-evaluate the match, or reload its namespace, so that it must handle " got ".")
                      {:type     ::not-parsed
                       :rule     ::rule/match-compiled-before-member
                       :expected (str (sum-name sum))
                       :handled  handled
                       :got      (.getName (class v))}))))
  ;; not parsed as any member, so this throws
  (parsed sum v))

(defn draft-of
  "A draft with the fields of `parsed`, a value [[parse]] issued. Child fields hold drafts of the children. Throws
  `::not-parsed` for any other value."
  [parsed]
  (let [plan (when (issued? parsed) (.get registry (class parsed)))]
    (when-not plan
      (throw (ex-info (if (instance? DraftValue parsed)
                        (str "pt/draft-of got " (describe parsed) ", which is already a draft. draft-of turns a value"
                             " pt/parse issued back into a draft. Use the draft as it is.")
                        (let [plan (when (instance? ParsedValue parsed) (.get registry (class parsed)))
                              [t d] (if plan
                                      [(.getSimpleName ^Class (:parses-to plan)) (.getSimpleName ^Class (:draft-class plan))]
                                      ["Debit" "DebitDraft"])]
                          (str "pt/draft-of got " (describe parsed)
                               (when (instance? ParsedValue parsed) " that pt/parse did not issue")
                               ". draft-of takes only a value pt/parse issued. Build a draft with its constructor"
                               " instead, as in (->" d " ...), or with (pt/draft " t " {...}).")))
                      {:type ::not-parsed
                       :rule (if (instance? DraftValue parsed) ::rule/draft-of-a-draft ::rule/draft-of-unissued)
                       :got  (some-> parsed class .getName)})))
    (let [values (reduce-kv (fn [vs field {:keys [kind]}]
                              (update vs field #(if (= :one kind) (some-> % draft-of) (mapv draft-of %))))
                            (into {} (map (fn [f] [f (get parsed f)])) (:fields plan))
                            (:children plan))]
      ((:make-draft plan) values))))

;;; ------------------------------------------------ Building and reading ------------------------------------------------

(defmacro draft
  "A draft of the parsed type `parsed-type`, defined with [[defparsed]], with the fields in `m`.

  `m` is a literal map that names every field, and only fields: anything else is a compile error, so write `nil` for
  a field you mean to leave empty. For values read at run time, name each field, as in `{:amount (:amount body)}`,
  or call `->NameDraft`.

    (pt/draft Debit {:user-id u, :account-id 1, :amount 30M})"
  [parsed-type m]
  (let [refuse! (fn [rule message data] (throw (ex-info message (merge {:type ::bad-input, :rule rule} data))))
        _       (when-not (symbol? parsed-type)
                  (refuse! ::rule/draft-type-not-a-symbol
                           (str "pt/draft: the type " (pr-str parsed-type) " is not a symbol. pt/draft names a parsed"
                                " type by its symbol. Write "
                                (if (or (string? parsed-type) (keyword? parsed-type))
                                  (pr-str (list 'pt/draft (symbol (name parsed-type)) m))
                                  "(pt/draft Debit {...})")
                                ".")
                           {:got (pr-str parsed-type)}))
        cls     (resolve parsed-type)
        _       (when (class? cls) (not-loaded! "pt/draft" cls))
        dcls    (when (and (parsed-type? cls) (.get registry cls)) (:draft-class (.get registry cls)))
        _       (when-not dcls
                  (refuse! ::rule/draft-not-a-parsed-type
                           (str "pt/draft: " parsed-type " is not a type defined with defparsed. pt/draft builds the"
                                " draft of a parsed type. Name a parsed type, not a sum or a draft, as in (pt/draft"
                                " Debit {...}).")
                           {:got (str parsed-type)}))
        dsym    (symbol (.getName ^Class dcls))
        flds    (:fields (.get registry cls))]
    (when-not (map? m)
      (refuse! ::rule/draft-not-a-literal-map
               (str "pt/draft: " (pr-str m) " is not a literal map. pt/draft checks the keys when it compiles, so it"
                    " takes a map written in place that names every field, and only fields. Write one, as in (pt/draft "
                    parsed-type " {" (clojure.string/join ", " (map #(str % " ...") flds)) "}), or call (->"
                    (.getSimpleName ^Class dcls) " ...).")
               {:got (pr-str m), :fields flds}))
    (when-let [unknown (seq (remove (set flds) (keys m)))]
      (refuse! ::rule/draft-literal-unknown-keys
               (str "pt/draft: " parsed-type " has no fields " (vec unknown) "."
                    (did-you-mean (first unknown) flds) " A literal map names only fields; its fields are "
                    flds ". Remove the others.")
               (cond-> {:unknown-fields (vec unknown), :fields flds}
                 (nearest (first unknown) flds) (assoc :suggestion (nearest (first unknown) flds)))))
    (when-let [missing (seq (remove (set (keys m)) flds))]
      (refuse! ::rule/draft-literal-missing-fields
               (str "pt/draft: " parsed-type " needs every field; missing " (vec missing) ". A literal map names"
                    " every field, so none is left out by mistake. Write nil for a field you mean to leave"
                    " empty, as in " (first missing) " nil.")
               {:missing (vec missing), :fields flds}))
    ;; bind in the order written, so the values are evaluated as they read
    (let [syms (into {} (map (fn [k] [k (gensym (name k))])) (keys m))]
      `(let [~@(mapcat (fn [[k v]] [(syms k) v]) m)]
         (new ~dsym ~@(map syms flds))))))
