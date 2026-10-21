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
  calling the world. What it guarantees: nothing fetches and no external check runs until every pure check of the
  draft passes.

  A field may hold values [[parse]] issued, but never a draft: parse the parts first, then the whole, as in
  `(pt/parse (pt/draft Order {:customer-id c, :lines (pt/parse-all line-drafts)}))`.

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
   [clojure.pprint :as pprint]
   [clojure.set :as set]
   [parsed-types.rule :as-alias rule]
   [clojure.string])
  (:import
   (java.lang.reflect Modifier)))

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

(defn- print-typed
  "Print `x`, a draft or parsed value, as `#ns.Type{...}`, like a record, so it is told apart from a plain map. The
  proof is not a key, so it is never printed. The reader cannot build one back: a map type is not a record."
  [x ^java.io.Writer w]
  (.write w (str "#" (.getName (class x))))
  (print-method (into {} x) w))

;; one method per interface, preferred over the map's, so a defparsed adds none
(defmethod print-method ParsedValue [x w] (print-typed x w))
(defmethod print-method DraftValue [x w] (print-typed x w))
(doseq [i [ParsedValue DraftValue]]
  (prefer-method print-method i clojure.lang.IPersistentMap)
  (prefer-method print-method i java.util.Map)
  (defmethod pprint/simple-dispatch i [x]
    (.write ^java.io.Writer *out* (str "#" (.getName (class x))))
    (pprint/simple-dispatch (into {} x)))
  (prefer-method pprint/simple-dispatch i clojure.lang.IPersistentMap))

;;; ------------------------------------------------ State ------------------------------------------------

;; Draft type -> its plan, and parsed type -> the same plan, by identity. Written once per class, when its `defparsed`
;; loads. It holds every class it was given, so a class a REPL replaces is never released: a REPL-only leak, of two
;; classes and a plan per re-evaluation.
(defonce ^:private ^java.util.concurrent.ConcurrentHashMap registry
  (java.util.concurrent.ConcurrentHashMap.))

;; Class name -> `{:ns :role :type}`: the namespace, as written, whose `defparsed` first claimed it, the role the class
;; has there, `:parsed` or `:draft`, and the parsed type's name. `my-app.bank` and `my_app.bank` compile to the same
;; Java names, and a type named `FooDraft` has the name of `Foo`'s draft, so without this one `defparsed` could
;; redefine the classes of another.
(defonce ^:private ^java.util.concurrent.ConcurrentHashMap owners
  (java.util.concurrent.ConcurrentHashMap.))

;; Parsed class name -> the fetch fns its `defparsed` evaluated and checked before defining any class, for `register!`
;; to take once the classes exist. A failed `defparsed` leaves its entry, which the next one replaces.
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
                                 {:rule    ::rule/no-metadata
                                  :message (str "with-meta on a " n ": a parsed value carries no metadata, so it never"
                                                " changes. Keep the metadata beside it, as in {:value x, :meta m}.")}
                                 "empty"
                                 {:rule    ::rule/no-empty
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
                                 {:rule    ::rule/no-metadata
                                  :message (str "with-meta on a " n ": a draft carries no metadata, so it holds only"
                                                " what parse checks. Keep the metadata beside it, as in {:value x,"
                                                " :meta m}.")}
                                 "empty"
                                 {:rule    ::rule/no-empty
                                  :message (str n " has fixed fields, so it has no empty version. Use (into {} x) for"
                                                " a plain map.")}
                                 {:rule    ::rule/draft-fields-are-fixed
                                  :message (str op (when (some? k) (str " " (pr-str k))) " on a " n ": a draft has"
                                                " fixed fields, set with assoc. Use (assoc d "
                                                (if (some? k) (pr-str k) ":field")
                                                (if (#{"dissoc" "remove"} op) " nil" " v") ").")})]
    (throw (ex-info message {:type ::cannot-change, :rule rule, :draft (.getName (class draft)), :op op, :key k}))))

(defn- no-field!
  "Throws `::no-field`: `x` has no field `k`. `op` is `\"read\"` when a key that is not a field is read, `\"read with a
  default\"` when it is read with one, or the change, such as `\"assoc\"` or `\"dissoc\"`, that named it. Called by the types [[defparsed]]
  defines."
  [x k op]
  (library-value! "parsed-types.core/no-field!" x)
  (let [fields  (vec (keys x))
        read?   (#{"read" "read with a default"} op)
        n       (.getSimpleName (class x))
        draft?  (instance? DraftValue x)
        nearest (nearest k fields)
        field   (pr-str (or nearest (first fields)))]
    (throw (ex-info (cond
                      (empty? fields)
                      (str n " has no field " (pr-str k) ": it has no fields at all, so every key "
                           (if read? "read throws, with a default too" "throws") ". Add the field to its"
                           " defparsed, as in (defparsed " (if draft? (subs n 0 (- (count n) 5)) n) " ["
                           (if (keyword? k) (name k) "field") "]).")

                      (= "read" op)
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A key that is not a field"
                           " throws, never reads nil. Read one of its fields " fields ", as in (" field " x).")

                      read?
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A key that is not a field"
                           " throws, even read with a default, so a misspelled key never reads the default. Read one"
                           " of its fields " fields ", as in (" field " x), or probe for the key with (contains? x "
                           (pr-str k) ") or (find x " (pr-str k) ").")

                      draft?
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A draft has fixed fields. Set"
                           " one of " fields ", as in (assoc d " field " v).")

                      :else
                      (str n " has no field " (pr-str k) "." (did-you-mean k fields) " A parsed value has fixed"
                           " fields, and never changes. Change one of " fields " on a copy, as in (pt/parse (assoc"
                           " (pt/draft-of x) " field " v))."))
                    (cond-> {:type                     ::no-field
                             :rule                     (if read?
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
  "The interfaces and methods of a read-only map with exactly `fields`. Reading another key throws, with a default
  too, but for `java.util.Map.get`, which returns null. `assoc-form`, over `this`, `k` and `v`, is what `assoc` does; `final-name`, the form of a fn, throws for
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
      ;; a default does not make a misspelled key read it: only a field reads
      (~'valAt [~this ~k ~nf] ~(lookup `((var no-field!) ~this ~k "read with a default")))

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
      ;; Java's contract, null for a key that is not a field, for Java code that probes keys: the one silent read
      (~'get [~this ~k] ~(lookup nil))
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
      ;; the printed form, which names the type, never the proof
      (~'toString [~this] (clojure.core/pr-str ~this))]))

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
    (syntax-error! form ::rule/field-name-unusable
                   (str "a field cannot be named /: the keyword :/ and the Java field it compiles to do not work as a"
                        " field. Rename it, as in slash.")))
  (when-let [special (seq (filter #(or (special-symbol? %) (= '& %)) fields))]
    (syntax-error! form ::rule/field-name-unusable
                   (str "a field cannot be named like a special form: " (vec special) ". A field is a local in the"
                        " type's methods, and a special form cannot be shadowed. Rename it, as in "
                        (first special) "-value.")))
  (when-let [dotted (seq (filter #(clojure.string/includes? (name %) ".") fields))]
    (syntax-error! form ::rule/field-name-unusable
                   (str "a field name cannot contain a dot: " (vec dotted) ". A dot is not allowed in a Java field."
                        " Use a dash instead, as in " (clojure.string/replace (name (first dotted)) "." "-") ".")))
  (when-let [clash (seq (for [[_ fs] (group-by #(munge (name %)) fields) :when (< 1 (count fs))] fs))]
    (syntax-error! form ::rule/field-name-unusable
                   (str "these fields compile to the same Java name: " (vec (first clash)) ". Each field needs its"
                        " own Java field. Rename one of them.")))
  (when-let [reserved (seq (filter '#{__meta __extmap __hash __hasheq} fields))]
    (syntax-error! form ::rule/field-name-unusable
                   (str "deftype reserves these field names: " (vec reserved) ". Rename them, as in "
                        (subs (name (first reserved)) 2) ".")))
  (when-let [reserved (seq (filter #(= "parse_proof" (munge (name %))) fields))]
    (syntax-error! form ::rule/field-name-unusable
                   (str "parsed-types reserves the field name parse-proof for the proof a parsed value carries: "
                        (vec reserved) ". Rename it, as in proof."))))

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
       (ns-unmap *ns* '~(symbol (str "->" name))))))

(defn- defdraft-type
  "The form that defines `name`, a bare draft type with `fields`. `assoc` sets a field; `dissoc` and `with-meta` throw,
  and so do reading or setting a key that is not a field. The positional factory `->NameDraft` is removed: [[draft]]
  builds a draft, naming every field. Only [[defparsed]] calls this, when it expands."
  [name fields]
  (clojure.core/let [this (gensym "this"), k (gensym "k"), v (gensym "v")]
    `(do
       (deftype ~name [~@fields]
         parsed_types.core.DraftValue
         ~@(map-type-methods fields this k v `((var assoc-draft) ~this ~k ~v) `(var fixed!) nil))
       ;; positional, so reordering the fields would silently swap values: pt/draft names every field instead
       (ns-unmap *ns* '~(symbol (str "->" name))))))

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

(defn- draft-hint
  "The code that builds a draft of the parsed class `c`, naming its fields, for an error message."
  [^Class c]
  (str "(pt/draft " (.getSimpleName c) " {"
       (clojure.string/join ", " (map #(str % " ...") (:fields (.get registry c))))
       "})"))

(defn- parse-hint
  "The code that makes a parsed value of `type`, a parsed class or a sum, for an error message."
  [type]
  (if (sum? type)
    (clojure.string/join " or " (map #(str "(pt/parse " (draft-hint %) ")") (sum-members type)))
    (str "(pt/parse " (draft-hint type) ")")))

(defn- stale-value!
  "Throws `::stale-type`: `what`, a value or draft of the parsed class `c`, is from before `c`'s `defparsed` was
  re-evaluated, and the code that got it expects the class defined since."
  [where what ^Class c]
  (throw (ex-info (str where " got " what " of " (.getName c) " from before " (defined-in c) " was re-evaluated. Types"
                       " compare by class, like records, and re-evaluating a defparsed makes new classes. Build it"
                       " again from a new draft, as in " (parse-hint c) ", or re-run the code that built it.")
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

(defn- missing-argument!
  "Throws `type` for `form`, a call of a library macro that leaves out an argument, with `message`, which says what is
  missing and the call to write."
  [form type message]
  (throw (ex-info message {:type type, :rule ::rule/missing-argument, :form (pr-str form)})))

(defmacro defsum
  "Define `name`, a closed set of parsed types. [[match]] on it must handle every member.

    (defsum Transaction [Debit Credit])"
  ([name]
   (missing-argument! &form ::bad-declaration
                      (str "(defsum " (pr-str name) ") lists no members. A sum names the parsed types it holds. List them"
                           " in a vector, as in (defsum " (pr-str name) " [Debit Credit]).")))
  ([name members]
   (when-not (simple-symbol? name)
     (throw (ex-info (str "defsum: the name " (pr-str name) " is not a symbol. A sum is a var, named by a symbol. Write"
                          " (defsum Transaction [Debit Credit]), with a symbol for the name.")
                     {:type ::bad-declaration, :rule ::rule/name-not-symbol, :name (pr-str name)})))
   ;; through `var`, because it is private
   `(def ~name ((var sum) '~name ~members))))

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
  ([sum-name]
   (missing-argument! &form ::bad-declaration
                      (str "(pt/match " (pr-str sum-name) ") has no value to match. match takes the sum, the value,"
                           " then a clause per member. Write (pt/match " (pr-str sum-name)
                           " x Debit d (f d) Credit c (g c)), with a clause for each member.")))
  ([sum-name x & clauses]
   (let [refuse! (fn [rule message data]
                   (throw (ex-info message (assoc data :type ::bad-declaration, :rule rule, :sum (pr-str sum-name)))))
         _       (when-not (symbol? sum-name)
                   (refuse! ::rule/name-not-symbol
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
                         {:type ::stale-type, :rule ::rule/stale-match, :sum (str sum-name), :class (name-of t)
                          :sum-ns sum-ns}))))
     ;; before the missing members: a clause that names a draft type is usually the member it misses
     (when (seq extra)
       (refuse! ::rule/match-not-a-member
                (str "match on " sum-name ": not members: " (mapv simple extra) ". A clause handles a member of the"
                     " sum. "
                     (if-let [[d p] (first (keep (fn [^Class c]
                                                   (when (.isAssignableFrom DraftValue c)
                                                     (some->> (.get registry c) :parses-to
                                                              (#(when (member? s %) [c %])))))
                                                 extra))]
                       (str (simple d) " is the draft of " (simple p) ", and a match branches on parsed values."
                            " Write " (simple p) " for " (simple d) ".")
                       "Remove those clauses, or add the types to the defsum."))
                {:extra (mapv name-of extra)}))
     (when (seq missing)
       (let [simple (mapv simple missing)]
         (refuse! ::rule/match-missing-member
                  (str "match on " sum-name " does not handle " simple ". A match handles every member, so adding"
                       " one to the sum breaks every match that misses it. Add a clause for each, as in "
                       (first simple) " x (f x).")
                  {:missing missing})))
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
                   '~(ns-name (.ns ^clojure.lang.Var sum-var)) ~v)))))))

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
                         (syntax-error! form ::rule/check-no-body
                                        (str what " ends after its fetch bindings " (pr-str (first more)) ". A check"
                                             " needs a body after its fetch bindings, which returns true to pass."
                                             " Write the body next, as in " (pr-str (first more)) " (some? "
                                             (first (first more)) ").")))
        [binds body]   (if (and external? (vector? (first more)) (next more))
                         [(first more) (rest more)]
                         [[] more])
        pairs          (partition-all 2 binds)]
    (when-not (qualified-keyword? n)
      (syntax-error! form ::rule/name-not-qualified
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
      (syntax-error! form ::rule/reads-unknown-field
                     (str what " reads fields the draft does not have: " (vec unknown) "."
                          (did-you-mean (first unknown) fields) " Use only its fields " fields ".")))
    (when-let [twice (seq (for [[f c] (frequencies args) :when (< 1 c)] f))]
      (syntax-error! form ::rule/field-named-twice
                     (str what " reads a field twice: " (vec twice) ". Name each field once.")))
    (when-not (every? #(and (= 2 (count %)) (symbol? (first %)) (contains? fetch-names (second %))) pairs)
      (syntax-error! form ::rule/check-bad-fetch-binding
                     (str what " binds " (pr-str binds) ". A check's fetches are pairs of a local and a fetch declared"
                          " in this defparsed. Write them like [account ::account], "
                          (if (seq fetch-names)
                            (str "naming one of " (vec (sort fetch-names)) ".")
                            "and declare the fetch, as in (fetch ::account [account-id] accounts-by-id)."))))
    (when-let [twice (seq (for [[l c] (frequencies (map first pairs)) :when (< 1 c)] l))]
      (syntax-error! form ::rule/fetch-local-taken
                     (str what " binds two fetches to one local: " (vec twice) ". Give each fetch its own local.")))
    (when-let [clash (seq (filter (set fields) (map first pairs)))]
      (syntax-error! form ::rule/fetch-local-taken
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
    (syntax-error! form ::rule/name-not-qualified
                   (str "the fetch " (pr-str n) ": a fetch's name must be a qualified keyword. Write it like ::"
                        (if (or (keyword? n) (symbol? n)) (name n) "account") ".")))
  (when-not (and (vector? reads) (every? (set fields) reads))
    (syntax-error! form ::rule/reads-unknown-field
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
  checks [[parse]] runs to turn a draft into a parsed value. Build a draft with [[draft]].

    (defparsed Order [customer-id lines subtotal]
      (pure ::lines-parsed \"Every line is a parsed OrderLine.\"
            [lines]
            (and (vector? lines) (every? #(pt/parsed? OrderLine %) lines)))
      (external ::customer-can-order \"The customer exists and may order.\"
                [] [customer ::customer]
                (true? (:can-order? customer)))
      (fetch ::customer [customer-id] customers-by-id))

  Clauses:

  - `(pure name doc [field ...] body ...)`: a check over the named fields.
  - `(external name doc [field ...] [local fetch-name ...]? body ...)`: a check over the named fields and the
    results of the named fetches.
  - `(fetch name [field ...] f)`: `f` takes every distinct tuple of the fields, and returns a map from each tuple to
    what it fetched. `f` is a fn, or a var that holds one, as in `#'rows-by-id`: a var is called through, so a test
    can stub it with `with-redefs`.

  A check takes no options. Its argument vector is the fields it reads: one that is not a field is a compile error.

  Pure checks run in the order written. A pure check is held, not run, when one before it that shares a field ran
  and failed. A held check holds nothing. A check whose body throws an `Exception` has failed: [[parse]] records it
  and runs the other checks of its phase. A pure check written before one that reads a proper subset of its fields
  is a compile error: move the smaller one first.

  External checks run in the order written, after every pure check passed. An external check is held when one
  before it that binds a fetch it binds ran and failed. So write the check that the row exists, or that the caller
  may see it, first: when it fails, no later check reports on that row.

  A field may hold values [[parse]] issued, which are not checked again, but never a draft: parse the parts first.

  Write `defparsed` at the top level of a namespace: inside another form it throws `::bad-declaration`.

  The plan [[parse]] follows is built and checked when the form loads, so every declaration error throws
  `::bad-declaration` then, not at the first parse. The fetch fns are evaluated and checked before any class is
  defined, so a re-evaluation they refuse leaves the type as it was. Only `defparsed` defines parsed types: the draft
  type and the parsed type are defined together, so the draft type is the only one that parses to its parsed type.

  Neither type has a positional factory: only [[draft]] builds a draft, naming every field, and only [[parse]]
  builds a value that [[parsed]] accepts."
  ([parsed-name]
   (missing-argument! &form ::bad-declaration
                      (str "(defparsed " (pr-str parsed-name) ") has no field vector. A defparsed names its type,"
                           " then its fields, then its clauses. Write (defparsed " (pr-str parsed-name) " [amount]),"
                           " or (defparsed " (pr-str parsed-name) " []) for a type with no fields.")))
  ([parsed-name fields & clauses]
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
     (when-not (and (seq? c) (symbol? (first c)) ('#{pure external fetch} (symbol (name (first c)))))
       (syntax-error! c ::rule/unknown-clause
                      (str (pr-str (if (seq? c) (first c) c)) " does not start a clause. A clause is (pure ...),"
                           " (external ...) or (fetch ...). Use one of those."))))
   (let [clauses     (map #(cons (symbol (name (first %))) (rest %)) clauses)
         fetches     (mapv #(parse-fetch-clause fields %) (filter (comp #{'fetch} first) clauses))
         fetch-names (set (map :name fetches))
         checks      (mapv #(parse-check-clause parsed-name fields fetch-names %)
                           (filter (comp '#{pure external} first) clauses))
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
     ;; when it expands, so that a refused defparsed never reaches its deftypes, which would redefine the classes
     (claim-class-names! (ns-name *ns*) parsed-name claims false)
     ;; A top-level `do` runs each form before it compiles the next, so a check that fails in the first form leaves
     ;; every class as it was. The private fns through `var`.
     `(do
        ;; the fetch fns, evaluated once and checked before any class is defined
        ((var stage!) ~parsed-class-name ~(str (namespace-munge *ns*) "." draft-name)
                      '~(mapv #(select-keys % [:name :reads]) fetches) [~@(map :f fetches)])
        ;; before the deftypes, which is what records the claim
        ((var claim-class-names!) '~(ns-name *ns*) '~parsed-name ~claims true)
        ~(defparsed-type parsed-name fields)
        ~(defdraft-type draft-name fields)
        ;; only this expansion may record a plan
        ((var register!) ~parsed-name ~draft-name
                         {:checks  ~checks
                          :fetches '~(mapv #(select-keys % [:name :reads]) fetches)}
                         ~(builders parsed-name draft-name fields))
        ~parsed-name))))

;;; ------------------------------------------------ Plans ------------------------------------------------

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
  "Check what [[defparsed]] cannot see when it expands, only when it loads, before it defines any class: a fetch that
  is not a fn, or a var that holds one. A var is called through, so `with-redefs` on it reaches the fetch. Throws `::bad-declaration`, so a `defparsed` that fails here changes nothing. Then keep `fetch-fns` for
  [[register!]]. `fetches` holds each fetch's `:name` and `:reads`, in the order of `fetch-fns`. Called by the
  expansion of [[defparsed]]."
  [parsed-class-name draft-class-name fetches fetch-fns]
  (doseq [[{n :name, :keys [reads]} f] (map vector fetches fetch-fns)]
    (when-not (or (fn? f) (and (var? f) (fn? @f)))
      (bad-declaration! draft-class-name ::rule/fetch-not-fn
                        (str "the fetch " n " is " (pr-str f) ", not a fn, in (fetch " n " " (mapv symbol reads) " "
                             (pr-str f) "). A fetch is called with the tuples it fetches. Write a fn of the tuples"
                             " it fetches, or a var that holds one, as in (fetch " n " " (mapv symbol reads)
                             " rows-by-id) or (fetch " n " " (mapv symbol reads) " #'rows-by-id).")
                        {:name n, :got (some-> f class .getName)})))
  (.put pending parsed-class-name fetch-fns)
  nil)

(defn- reads-set [x] (set (:reads x)))

(defn- compile-plan
  "Order the checks, and work out what holds each.

  - A pure check is held when a pure check before it that shares a field ran and failed, or threw: one held in turn
    holds nothing.
  - An external check is held when any pure check failed or threw, and fetches run only when none did, so malformed input
    never reads the world.
  - An external check is held when an external check before it that binds a fetch it binds ran and failed, or threw,
    so a failed permission check written first keeps the checks after it from reporting on the row it refused.

  Pure checks run first, in the order written, then fetches, then external checks, each in the order written.
  [[defparsed]] refuses a pure check written before one that reads a proper subset of its fields, so a check on one
  field runs before a check relating it to another."
  [parses-to draft-class fields {:keys [checks], :as decl}]
  (let [pure      (filterv #(= :pure (:kind %)) checks)
        pure'     (map-indexed (fn [i c]
                                 (assoc c :prereqs (into #{}
                                                         (comp (take i)
                                                               (filter #(seq (set/intersection (reads-set %)
                                                                                               (reads-set c))))
                                                               (map :name))
                                                         pure)))
                               pure)
        external  (filterv #(= :external (:kind %)) checks)
        external' (map-indexed (fn [i c]
                                 (assoc c :prereqs (into (set (map :name pure))
                                                         (comp (take i)
                                                               (filter #(seq (set/intersection (set (:fetches %))
                                                                                               (set (:fetches c)))))
                                                               (map :name))
                                                         external)))
                               external)]
    {:draft-class    draft-class
     :parses-to      parses-to
     :declaration    (assoc decl :parses-to parses-to)
     :fields         fields
     :pure           (vec pure')
     :fetches        (:fetches decl)
     :external       (vec external')
     :checks-by-name (into {} (map (juxt :name identity)) (concat pure' external'))}))

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
  [[defparsed]] generated and the fetch fns [[stage!]] kept. Called by the expansion of [[defparsed]], when it loads,
  after its `deftype`s."
  [parsed-type draft-type decl builders]
  (let [fetch-fns (.remove pending (.getName ^Class parsed-type))
        decl      (update decl :fetches #(mapv (fn [fd f] (assoc fd :f f)) % fetch-fns))
        plan      (merge (compile-plan parsed-type draft-type (:fields builders) decl) builders)]
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
                                  (call (draft-hint (class draft))) ".")

                             :else
                             (str " Build a draft with pt/draft, naming every field, as in "
                                  (call "(pt/draft Debit {:user-id u, :account-id a, :amount n})") ".")))
                      {:type ::bad-input, :rule ::rule/not-a-draft, :fn fn-name, :got (some-> draft class .getName)}))))
  (.get registry (class draft)))

(defn declaration
  "The declaration `defparsed` wrote for `draft`'s type: `{:parses-to :checks :fetches}`, as data. Throws
  `::bad-input` for anything but a draft."
  [draft]
  (:declaration (plan-for "pt/declaration" draft)))

;;; ------------------------------------------------ Running ------------------------------------------------

(def ^:private ^Class default-path-class
  "The class of a `java.nio.file.Path` of the default file system, which the JDK documents as immutable. A `Path` of
  another file system, such as a zip file's, is tied to a file system that can close, and is not accepted."
  (class (.getPath (java.nio.file.FileSystems/getDefault) "" (make-array String 0))))

(def ^:private ^java.util.Set immutable-classes
  "Classes whose values never change, by exact class: `BigDecimal` and `BigInteger` can be subclassed, so a subclass is
  not trusted. A `java.util.Set`, whose lookup of a class is the walk's most frequent call."
  (java.util.Set/of (object-array [Boolean Character String clojure.lang.Keyword clojure.lang.Symbol java.util.UUID
                                   java.util.regex.Pattern Long Integer Short Byte Double Float BigDecimal BigInteger
                                   clojure.lang.BigInt clojure.lang.Ratio java.net.URI java.util.Locale
                                   java.util.Currency default-path-class])))

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

(defn- leaf?
  "Whether `x` holds no value and carries no metadata: nil, or a value of [[immutable-classes]] that cannot have any,
  such as a number, a string or a keyword. The walk visits no frame for one."
  [x]
  (or (nil? x) (and (.contains immutable-classes (class x)) (not (instance? clojure.lang.IObj x)))))

(defn- add-frame
  "`frames`, a transient vector, with a frame to visit `x`, at `step` inside the value at `path`, or as it is when `x`
  is a [[leaf?]]: a leaf only counts one level in `acc`, the height of the value that holds it. When `deep?`, a value
  inside would sit past the depth limit, so a leaf is visited too, and refused."
  [frames x step path ^longs acc deep?]
  (if (and (not deep?) (leaf? x))
    (do (aset acc 0 (max (aget acc 0) 1)) frames)
    (conj! frames [:visit x (conj path step) acc])))

(defn- frames-of
  "`[:walk frames]` for `v`, at `path`: a frame for its metadata, then for each of `items`, the values inside it,
  through their own iterator, so [[add-frame]] is called from one place: a reduce would share its call site with
  every other reduce in the program. `kind` is `:map`, `:indexed`, `:set`, or `:value` for the value an `Optional`
  holds."
  [v ^Iterable items kind path ^longs acc]
  (let [deep? (<= max-value-depth (count path))
        m     (when (instance? clojure.lang.IObj v) (meta v))
        it    (.iterator items)]
    (loop [fs (cond-> (transient []) (some? m) (conj! [:visit m (conj path '<meta>) acc])), i 0]
      (if (.hasNext it)
        (let [x (.next it)]
          (recur (case kind
                   :map     (let [k (key x)]
                              (-> fs (add-frame k '<key> path acc deep?) (add-frame (val x) k path acc deep?)))
                   :indexed (add-frame fs x i path acc deep?)
                   :set     (add-frame fs x '<element> path acc deep?)
                   :value   (add-frame fs x '<value> path acc deep?))
                 (inc i)))
        [:walk (persistent! fs)]))))

(defn- children-or-refusal
  "What the walk in [[not-immutable]] does with `v`, at `path`: `[:refuse rule what fix]`, or `[:walk frames]`, a
  vector of frames to visit for the values inside it, each with `acc`, which collects `v`'s height. Collections are
  tested first, as the most common."
  [v path ^longs acc]
  (let [c (class v)]
    (cond
      (.contains immutable-classes c) (frames-of v [] :indexed path acc) ; a symbol may carry metadata

      ;; an issued value proved its own checks, and holds only immutable values, so it is not walked again
      (instance? ParsedValue v)
      (if (issued? v)
        [:walk []]
        [:refuse ::rule/unissued-in-field (str "a value of " (.getSimpleName c) " that pt/parse did not issue")
         (if (parsed-type? c)
           (str "Pass the value pt/parse returns instead, as in " (parse-hint c) ".")
           (str "A class that only implements ParsedValue is never parsed: pass a value of a type defparsed defined,"
                " as pt/parse returns it."))])

      (instance? DraftValue v)
      [:refuse ::rule/draft-in-field "a draft"
       "Parse it first, as in (pt/parse d), or each of a vector of drafts with (pt/parse-all ds)."]

      (and (instance? clojure.lang.Sorted v)
           (not (identical? clojure.lang.RT/DEFAULT_COMPARATOR (.comparator ^clojure.lang.Sorted v))))
      [:refuse ::rule/mutable-value "a sorted collection with a custom comparator, which can read state that changes"
       "Use (sorted-map ...) or (sorted-set ...), which sort with compare, or a plain map or set."]

      (instance? clojure.lang.IPersistentMap v) (frames-of v v :map path acc)
      (or (instance? clojure.lang.IPersistentVector v) (list? v)
          (instance? clojure.lang.PersistentQueue v)) (frames-of v v :indexed path acc)
      (instance? clojure.lang.IPersistentSet v) (frames-of v v :set path acc)

      (class? v) [:walk []]

      ;; a constant of a Java enum, as java.util.concurrent.TimeUnit/SECONDS
      (instance? Enum v) [:walk []]

      (instance? java.util.Optional v)
      (let [^java.util.Optional o v] (frames-of v (if (.isPresent o) [(.get o)] []) :value path acc))

      (some #(.isInstance ^Class % v) clock-classes)
      [:refuse ::rule/mutable-value "a clock, which reads the time each time it is asked"
       "Pass the instant it reads, as in (.instant clock)."]

      (java-time-value? c) [:walk []]

      (instance? java.net.URL v)
      [:refuse ::rule/url-value "a java.net.URL, whose equals and hashCode look its host up over the network"
       "Use a java.net.URI, as in (.toURI url)."]

      :else
      (let [[what fix]
            (cond
              ;; not (vec a): vec of an Object[] of 32 or fewer elements shares the array
              (.isArray c)                        ["a Java array" "Copy it into a vector, as in (into [] a)."]
              (instance? clojure.lang.IAtom v)    ["an atom" "Deref it, as in @a."]
              (instance? clojure.lang.IRef v)     ["a reference type" "Deref it, as in @r."]
              (instance? clojure.lang.Volatile v) ["a volatile" "Deref it, as in @v."]
              ;; java.sql.Date and java.sql.Time throw on .toInstant
              (instance? java.sql.Date v)         ["a mutable date" "Use java.time.LocalDate, as in (.toLocalDate d)."]
              (instance? java.sql.Time v)         ["a mutable time" "Use java.time.LocalTime, as in (.toLocalTime d)."]
              (instance? java.util.Date v)        ["a mutable date" "Use java.time.Instant, as in (.toInstant d)."]

              (instance? clojure.lang.LazySeq v)
              ["a lazy seq, which realizing could run side effects or never end" "Pass a vector, as in (vec s)."]

              (instance? clojure.lang.ISeq v)     ["a seq, which may be lazy" "Pass a vector, as in (vec s)."]

              (or (instance? java.util.Collection v) (instance? java.util.Map v))
              ["a mutable Java collection" "Copy it into a Clojure collection, as in (vec c) or (into {} m)."])]
        (if what
          [:refuse ::rule/mutable-value what fix]
          [:refuse ::rule/not-known-immutable "a value of a class not known to be immutable"
           "Convert it to a value of an accepted class, as in (str x)."])))))

(defn- not-immutable
  "nil when `v` is known to be immutable all the way down, else `[rule path what fix class-name]`: the rule, where
  inside the field the first value that is not is, what it is, the fix, and its class. docs/guarantees.md lists what
  it accepts. Accepts nil, the values of [[immutable-classes]], classes, Java enum constants, an `Optional` whose
  value passes, `java.time` values but clocks, values [[parse]] issued, and persistent vectors, maps
  (records included), sets, lists and queues whose contents pass, map keys and metadata included. Refuses a draft,
  which no check of this parse looks inside, and a parsed type's value `parse` did not issue. A sorted
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
                  known (when (instance? clojure.lang.IObj x) (.get heights x))]
              (cond
                (< max-value-depth depth)
                [::rule/value-too-deep path (str "a value nested deeper than " max-value-depth " levels")
                 (str "Store a flatter value: for nested vectors, (vec (flatten v)), or keep it as text, as in"
                      " (pr-str v).")
                 ;; x may be nil, nested past the limit
                 (if (nil? x) "nil" (.getName (class x)))]

                ;; the root, when it is a leaf: a leaf inside a value gets no frame
                (leaf? x)
                (do (bump! parent 0) (recur stack))

                (and known (<= (+ depth (long known)) max-value-depth))
                (do (bump! parent known) (recur stack))

                :else
                (let [acc             (long-array 1)
                      [result & more] (children-or-refusal x path acc)]
                  (if (= :refuse result)
                    (let [[rule what fix] more] [rule path what fix (.getName (class x))])
                    (let [frames (first more)]
                      (if (empty? frames)
                        ;; only leaves inside, or none: its height is already in acc
                        (let [h (aget acc 0)]
                          (when (pos? h) (.put heights x h))
                          (bump! parent h)
                          (recur stack))
                        (recur (into (conj stack [:exit x acc parent]) (rseq frames)))))))))))))))

(defn- refuse-mutable!
  "Throws `::bad-input` when a field of `draft` holds, anywhere inside it, a value that is not known to be immutable,
  or a draft or an unissued value: the checks would prove a value the parsed value does not keep. There is no
  opt-out. `walked` is as for [[not-immutable]]."
  [draft values walked]
  (doseq [[field v] values
          :let        [[rule path what fix got] (not-immutable v walked)]
          :when       what]
    (let [shown (if (< 8 (count path)) (str (subvec path 0 8) " and " (- (count path) 8) " levels more") path)]
      (throw (ex-info (str (.getSimpleName (class draft)) ": the field " field
                           (when (seq path) (str " at " shown))
                           " holds " what " (" got "). "
                           (case rule
                             ::rule/draft-in-field
                             "A field holds values pt/parse issued, never drafts: parse checks only the draft it is given."
                             ::rule/unissued-in-field
                             "A field holds values pt/parse issued, and this one carries no proof."
                             ::rule/url-value
                             "Parsed values compare with =, and comparing two URLs could wait on the network."
                             ::rule/not-known-immutable
                             (str "A parsed value holds only values of the classes the library knows to be immutable,"
                                  " so what its checks proved stays true.")
                             (str "A parsed value holds only values known to be immutable, so what its checks proved"
                                  " stays true"
                                  (when (= rule ::rule/value-too-deep) ", and proving it walks a bounded depth")
                                  "."))
                           " " fix)
                      (merge {:type ::bad-input, :rule rule, :draft (.getName (class draft)), :field field
                              :path path, :got got}
                             ;; a path over the limit is long: the ex-data keeps its first steps and its length
                             (when (= rule ::rule/value-too-deep)
                               {:path (subvec path 0 8), :depth (count path)})))))))

(defn- start [fn-name draft walked]
  (let [plan   (plan-for fn-name draft)
        ;; every field read once, so the checks and the parsed value see the same values
        values ((:read-draft plan) draft)]
    (refuse-mutable! draft values walked)
    {:draft   draft
     :values  values
     :plan    plan
     :fetched {}
     :failed  {}     ; check name -> failure entry
     :order   []}))  ; failed check names, in the order they failed

(defn- ran-and-failed?
  "Whether `entry`, a failure entry, is of a check that ran: it returned a failure, or threw. A held check did neither."
  [entry]
  (or (contains? entry :returned) (contains? entry :threw)))

(defn- roots-of
  "The checks among `prereqs` that ran and failed, or nil. A held check did not run, so it holds nothing."
  [st prereqs]
  (seq (filter #(ran-and-failed? (get-in st [:failed %])) prereqs)))

(defn- block
  "`st` with the check `n` listed under `:blocked` of each of `roots`, the checks that held it."
  [st n roots]
  (reduce #(update-in %1 [:failed %2 :blocked] (fnil conj []) n) st roots))

(defn- call-check
  "`{:returned x}`, what the check fn `f` returned on `args`, or `{:threw e}` for the `Exception` its body threw. Only
  the call is inside the `try`, so an exception of the library's own is never recorded. An `Error` propagates, and so
  does an `InterruptedException`, once the thread's interrupt flag, which throwing it cleared, is set again."
  [f args]
  (try {:returned (apply f args)}
       (catch InterruptedException e
         (.interrupt (Thread/currentThread))
         (throw e))
       (catch Exception e
         {:threw e})))

(defn- run-check [st {n :name, :keys [prereqs f doc reads fetches]}]
  (if-let [roots (roots-of st prereqs)]
    (block st n roots)
    (let [result (call-check f (into (mapv (:values st) reads) (map (:fetched st)) fetches))]
      (if (true? (:returned result))
        st
        (-> st
            (assoc-in [:failed n] (assoc result :doc doc))
            (update :order conj n))))))

(defn- fetch-phase
  "Run every fetch the drafts that passed their pure checks need, each once per plan across all of them, with each
  distinct tuple of arguments once."
  [sts]
  (let [wanted (for [[i st] (map-indexed vector sts)
                     :when  (empty? (:failed st))
                     fd     (get-in st [:plan :fetches])]
                 {:i i, :fd fd})
        tuple  (fn [{:keys [i fd]}] (mapv (:values (nth sts i)) (:reads fd)))]
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
                            :rule    ::rule/fetch-missing-tuples
                            :fetch   (:name fd)
                            :missing (vec missing)})))
         (reduce (fn [sts w]
                   (assoc-in sts [(:i w) :fetched (:name fd)] (get result (tuple w))))
                 sts ws)))
     sts
     ;; one call per fetch per plan: the same fetch name in two declarations is two fetches
     (group-by (fn [{:keys [i fd]}] [(get-in sts [i :plan]) (:name fd)]) wanted))))

(defn- run-all
  "Run the checks of every draft, and return the state of each. `fn-name` names the fn that got them."
  [fn-name drafts]
  (let [walked (java.util.IdentityHashMap.)
        ;; every draft is read and walked before any check runs
        sts    (mapv #(start fn-name % walked) drafts)
        sts    (mapv #(reduce run-check % (get-in % [:plan :pure])) sts)
        ;; a draft that failed a pure check fetches nothing, and its external checks are held
        sts    (fetch-phase sts)]
    (mapv #(reduce run-check % (get-in % [:plan :external])) sts)))

(defn- failed-kinds
  "The kinds, `:pure` or `:external`, of the checks that returned a failure or threw in `sts`. A held check did
  neither: what holds it failed."
  [sts]
  (into #{}
        (for [{:keys [plan failed]} sts
              [n entry]             failed
              :when                 (ran-and-failed? entry)]
          (:kind ((:checks-by-name plan) n)))))

(defn- failed-names
  "Each check that returned a failure or threw in `st`, in the order they ran, as `[name check entry]`: the check of
  the plan, and its failure entry."
  [{:keys [failed order plan]}]
  (for [n order :when (ran-and-failed? (failed n))] [n ((:checks-by-name plan) n) (failed n)]))

(defn- threw-names
  "The set of the checks that threw in `st`."
  [{:keys [failed order]}]
  (into #{} (filter #(contains? (failed %) :threw)) order))

(defn- first-thrown
  "The first exception a check threw in `sts`, in the order the checks ran: every draft's pure checks, in draft order,
  then every draft's external checks. A draft whose pure check threw runs no external check. nil when none threw."
  [sts]
  (let [thrown (fn [kind]
                 (for [{:keys [failed order plan]} sts
                       n                           order
                       :let                        [e (:threw (failed n))]
                       :when                       (and e (= kind (:kind ((:checks-by-name plan) n))))]
                   e))]
    (first (concat (thrown :pure) (thrown :external)))))

(defn- truthy-hint
  "A hint for a check that returned `x`, a value that is truthy but not `true`, as `(seq xs)` does: it fails, which is
  rarely what the body meant. Empty for nil, false, a string or a map, which a check returns on purpose to say why it
  failed, and for a number, which `(boolean ...)` would turn into a check that always passes."
  [x]
  (if (or (not x) (true? x) (string? x) (map? x) (number? x))
    ""
    (str ", which returned " (binding [*print-length* 5, *print-level* 3] (pr-str x)) ", which is truthy but not"
         " true; wrap the body in (boolean ...)")))

(defn- threw-fix
  "The fix for `check`, a check of a plan that threw: for a pure check, a pure check written before it that guards its
  first field; for an external check, a guard in its body for a fetch that found nothing."
  [{:keys [kind reads]}]
  (cond
    (= :external kind)
    (str " An external check gets nil when a fetch found nothing, so guard it in the body, as in (and (some? row)"
         " (:ok? row)), and guard the fields it reads with a pure check, which runs first.")

    (seq reads)
    (let [f (name (first reads))]
      (str " Guard its input with a pure check written before it, as in (pure ::" f "-number \"" f " is a number.\" ["
           f "] (number? " f ")): a pure check that fails holds every later check that reads " f "."))

    :else
    " Fix its body so that it returns true or false."))

(defn- issue
  "The parsed value of `st`, whose checks all passed: built with the proof, from the fields the checks saw."
  [{:keys [plan values]}]
  ((:make-parsed plan) values parse-token))

(defn- invalid!
  "Throws `::invalid` for `what`, naming each failed check of `named`, `[label check entry]` triples, with its doc and
  what it threw: the first five, then how many more. The first exception a check threw in `sts` is the cause."
  [what named sts data]
  (let [shown  (take 5 named)
        more   (- (count named) (count shown))
        cause  (first-thrown sts)
        thrown (when cause (first (filter (fn [[_ _ entry]] (identical? cause (:threw entry))) named)))]
    (throw (ex-info (str "Invalid " what ": " (count named) (if (= 1 (count named)) " check" " checks") " failed: "
                         (clojure.string/join ", " (map (fn [[label {:keys [doc]} entry]]
                                                         (if-let [^Throwable e (:threw entry)]
                                                           (str label " (" doc ") threw " (.getSimpleName (class e))
                                                                ": " (ex-message e))
                                                           (str label " (" doc ")" (truthy-hint (:returned entry)))))
                                                       shown))
                         (when (pos? more) (str ", and " more " more"))
                         ". A draft parses only when every check returns exactly true."
                         (if thrown
                           (str " A check that throws met input that no check before it refused: "
                                (first thrown) " threw." (threw-fix (second thrown)) " (:threw (ex-data e)) names"
                                " every check that threw, and (:failures (ex-data e)) has what each returned or threw.")
                           (str " Change the draft so they pass and parse again; (:failures (ex-data e)) has what each"
                                " returned.")))
                    (merge {:type ::invalid, :rule ::rule/checks-failed, :failed-kinds (failed-kinds sts)} data)
                    cause))))

;;; ------------------------------------------------ API ------------------------------------------------

(defn- assoc-draft
  "`draft` with `field` set to `v`: a new draft. This is what `assoc` on a draft does; call `assoc`."
  [draft field v]
  (let [{:keys [fields make-draft read-draft]} (plan-for "assoc" draft)]
    (when-not (some #{field} fields)
      (no-field! draft field "assoc"))
    (make-draft (assoc (read-draft draft) field v))))

(defn parse-all
  "Parse each draft. Returns a vector of parsed values aligned with `drafts`, or throws `::invalid` once with every
  failure, keyed by the index of each draft that failed:

  - `:failures`: index -> that draft's failures, shaped like [[parse]]'s.
  - `:threw`: index -> the set of that draft's checks that threw, for each draft where one did; `{}` when none did.
  - `:failed-kinds`: the kinds of check that failed or threw in any of them.

  One draft's check that throws stops no other draft's checks. The first exception a check threw, in the order they
  ran, is the cause. Each fetch runs once for the whole call, with each distinct tuple once, so parsing the parts of a
  whole together costs one call per fetch. Exceptions out of a fetch, an `Error` and an `InterruptedException`
  propagate. Throws `::bad-input` when `drafts` is a single draft, a map, or not a sequential collection."
  [drafts]
  (when (or (map? drafts) (not (sequential? drafts)))
    (throw (ex-info (str "pt/parse-all got " (describe drafts) ". It takes a vector of drafts and returns a vector of"
                         " parsed values. Pass a vector, as in (pt/parse-all [draft]), or parse one draft with"
                         " (pt/parse draft).")
                    {:type ::bad-input, :rule ::rule/parse-all-not-a-vector, :got (some-> drafts class .getName)})))
  (let [sts      (run-all "pt/parse-all" drafts)
        failures (into (sorted-map) (keep-indexed (fn [i st] (some->> (not-empty (:failed st)) (vector i)))) sts)]
    (when (seq failures)
      (invalid! (str "drafts at indexes " (vec (keys failures)))
                (for [i (keys failures), [n check entry] (failed-names (nth sts i))] [(str n " in draft " i) check entry])
                sts
                {:failures failures
                 :threw    (into (sorted-map) (keep-indexed (fn [i st] (some->> (not-empty (threw-names st)) (vector i))))
                                 sts)}))
    (mapv issue sts)))

(defn parse
  "Parse `draft`, running every check. Returns a new value of its parsed type. When a check fails or throws, throws
  `::invalid` with:

  - `:failures`: every failure, by check name: `{:doc :returned :blocked}`, or `{:doc :threw :blocked}` for a check
    whose body threw an `Exception`, which `:threw` holds.
  - `:threw`: the set of the checks that threw, `#{}` when none did. One that is not empty is a bug in a check, not
    bad input.
  - `:failed-kinds`: a set of `:pure` and `:external`, the kinds of check that failed or threw. A pure failure means
    the input is malformed; an external one means a check of the world refused it.
  - `:draft`: the name of the draft's class.

  A check that throws holds later checks as a failure does, and the other checks of its phase still run. The first
  exception a check threw is the cause. Exceptions out of a fetch, an `Error` and an `InterruptedException`
  propagate."
  [draft]
  (let [[st] (run-all "pt/parse" [draft])]
    (when (seq (:failed st))
      (invalid! (.getSimpleName ^Class (:parses-to (:plan st))) (failed-names st) [st]
                {:draft (.getName (class draft)), :failures (:failed st), :threw (threw-names st)}))
    (issue st)))

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

(defn- issued-member?
  "Whether `x` is a value [[parse]] issued, of `type` or of a member of the sum `type`, already checked."
  [type x]
  (and (some? x) (member? type (class x)) (issued? x)))

(defn parsed?
  "Whether `x` is a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`: a value of
  the class itself, as for a record, whose hidden proof field holds the proof only `parse` sets. Throws `::bad-input`
  when `parsed-type` is neither. Throws `::stale-type` when `x` is a value `parse` issued of a class with the name of
  the type but not the class: one of the two is from before a REPL re-evaluated its `defparsed`, and the message
  says which to re-evaluate."
  [parsed-type x]
  (check-type! "pt/parsed?" parsed-type)
  (stale-sink! "pt/parsed?" parsed-type x)
  (issued-member? parsed-type x))

(defn parsed
  "`x`, which must be a value that [[parse]] issued, of `parsed-type` or of a member of the sum `parsed-type`. Throws
  `::not-parsed` otherwise, and `::stale-type` as [[parsed?]] says. Read its fields by keyword, as in `(:amount x)`: a
  key that is not a field throws `::no-field`. Throws `::bad-input` when `parsed-type` is not a parsed type or a sum."
  [parsed-type x]
  (check-type! "pt/parsed" parsed-type)
  (stale-sink! "pt/parsed" parsed-type x)
  ;; not parsed?, which would check the type and the class again
  (when-not (issued-member? parsed-type x)
    (let [t                      (type-name parsed-type)
          {:keys [rule message]} (cond
                                   (nil? x)
                                   {:rule    ::rule/got-unparsed
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
                                   {:rule    ::rule/got-unparsed
                                    :message (str "Expected a parsed " t ", got " (describe x) " that pt/parse did"
                                                  " not issue. A value built with its constructor or by hand"
                                                  " carries no proof. Write " (parse-hint parsed-type)
                                                  " and pass the result.")}

                                   (and (map? x) (not (instance? ParsedValue x)) (not (instance? DraftValue x)))
                                   {:rule    ::rule/got-unparsed
                                    :message (str "Expected a parsed " t ", got a plain map. A sink takes only a"
                                                  " value pt/parse issued. Build a draft from its fields and parse"
                                                  " it, as in " (parse-hint parsed-type) ", and pass the result.")}

                                   :else
                                   {:rule    ::rule/got-unparsed
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
  "A draft with the fields of `parsed`, a value [[parse]] issued. Throws `::not-parsed` for any other value."
  [parsed]
  (let [plan (when (issued? parsed) (.get registry (class parsed)))]
    (when-not plan
      (throw (ex-info (if (instance? DraftValue parsed)
                        (str "pt/draft-of got " (describe parsed) ", which is already a draft. draft-of turns a value"
                             " pt/parse issued back into a draft. Use the draft as it is.")
                        (str "pt/draft-of got " (describe parsed)
                             (when (instance? ParsedValue parsed) " that pt/parse did not issue")
                             ". draft-of takes only a value pt/parse issued. Build a draft with pt/draft instead, as in "
                             (if (parsed-type? (class parsed))
                               (draft-hint (class parsed))
                               "(pt/draft Debit {:user-id u, :account-id a, :amount n})")
                             "."))
                      {:type ::not-parsed
                       :rule (if (instance? DraftValue parsed) ::rule/draft-of-a-draft ::rule/draft-of-unissued)
                       :got  (some-> parsed class .getName)})))
    ((:make-draft plan) (into {} (map (fn [f] [f (get parsed f)])) (:fields plan)))))

;;; ------------------------------------------------ Building and reading ------------------------------------------------

(defmacro draft
  "A draft of the parsed type `parsed-type`, defined with [[defparsed]], with the fields in `m`.

  `m` is a literal map that names every field, and only fields: anything else is a compile error, so write `nil` for
  a field you mean to leave empty. For values read at run time, name each field, as in `{:amount (:amount body)}`.
  This is the only way to build a draft: there is no positional `->NameDraft`.

    (pt/draft Debit {:user-id u, :account-id 1, :amount 30M})

  The values are evaluated in the order Clojure evaluates the same map literal: as written for up to 8 keys, and in
  no order the source shows beyond that."
  ([parsed-type]
   (missing-argument! &form ::bad-input
                      (str "(pt/draft " (pr-str parsed-type) ") has no map of fields. pt/draft takes the type, then a"
                           " literal map that names every field. Write "
                           (let [c (when (symbol? parsed-type) (resolve parsed-type))]
                             (if (parsed-type? c) (draft-hint c) "(pt/draft Debit {:user-id ..., :amount ...})"))
                           ".")))
  ([parsed-type m]
   (let [refuse! (fn [rule message data] (throw (ex-info message (merge {:type ::bad-input, :rule rule} data))))
         _       (when-not (symbol? parsed-type)
                   (refuse! ::rule/name-not-symbol
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
                     parsed-type " {" (clojure.string/join ", " (map #(str % " ...") flds)) "}).")
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
     ;; bind in the map's own order, the order Clojure evaluates the same map literal in: as written for up to 8 keys,
     ;; and the reader's hash order beyond, which the source no longer holds
     (let [syms (into {} (map (fn [k] [k (gensym (name k))])) (keys m))]
       `(let [~@(mapcat (fn [[k v]] [(syms k) v]) m)]
          (new ~dsym ~@(map syms flds)))))))
