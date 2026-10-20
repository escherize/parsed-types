(ns parsed-types.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;; What ran, in order.
(def ^:private ran (atom []))

(defn- log! [x] (swap! ran conj x) true)

(def ^:private writable-card-ids #{1 2})

(defn- card-rows [arg-tuples]
  (log! [::card arg-tuples])
  (into {} (map (fn [[id :as t]] [t {:id id, :writable? (contains? writable-card-ids id)}])) arg-tuples))

(pt/defparsed RenameCard [user-id card-id title]
  (pure ::card-id-positive "The card id is a positive integer."
        [card-id]
        (log! ::card-id-positive)
        (pos-int? card-id))
  (pure ::title-not-blank "The title is a non-blank string."
        [title]
        (log! ::title-not-blank)
        (or (and (string? title) (not (str/blank? title)))
            {:title title}))
  (pure ::title-not-the-card-id "The title is not the card id."
        [card-id title]
        (log! ::title-not-the-card-id)
        (not= (str card-id) title))
  (external ::may-write-card "User 1 may write the card."
            [user-id] [card ::card]
            (log! ::may-write-card)
            (and (= 1 user-id) (:writable? card)))
  (fetch ::card [card-id] card-rows))

(pt/defparsed Other [user-id card-id title])

(defn- thrown-data [thunk]
  (try (thunk) nil
       (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- runs [thunk]
  (reset! ran [])
  (thunk)
  @ran)

(deftest parse-test
  (testing "every check passes: parse returns a new value of the parsed type"
    (let [draft  (->RenameCardDraft 1 1 "New")
          parsed (pt/parse draft)]
      (is (instance? RenameCard parsed))
      (is (identical? parsed (pt/parsed RenameCard parsed)))
      (is (false? (pt/parsed? RenameCard draft)) "the draft is not the parsed value")
      (testing "it reads as a map and through its fields"
        (is (= {:user-id 1, :card-id 1, :title "New"} (into {} parsed)))
        (is (not= {:user-id 1, :card-id 1, :title "New"} parsed) "a parsed value never equals a plain map")
        (is (= parsed (pt/parse (->RenameCardDraft 1 1 "New"))) "it equals another parsed value with the same fields")
        (is (not= parsed (RenameCard. 1 1 "New" nil)) "and never a value of its type built without the proof")
        (is (not= parsed (->RenameCardDraft 1 1 "New")) "and not its draft")
        (is (= 1 (:card-id parsed)))
        (is (= 1 (.-card-id ^RenameCard parsed)))
        (is (= ::pt/no-field (:type (thrown-data #(:nope parsed)))) "a key that is not a field throws")
        (is (= ::none (get parsed :nope ::none)) "unless a default is given")
        (is (not (contains? parsed :nope))))
      (testing "it prints with its type"
        (is (= "#parsed_types.core_test.RenameCard{:user-id 1, :card-id 1, :title \"New\"}" (pr-str parsed))))))
  (testing "a failed pure check is reported by name, with its doc and what it returned"
    (is (= {:type         ::pt/invalid
            :rule         :parsed-types.rule/checks-failed
            :failed-kinds #{:pure}
            :draft        "parsed_types.core_test.RenameCardDraft"
            :failures    {::card-id-positive {:doc      "The card id is a positive integer."
                                              :returned false
                                              :blocked  [::title-not-the-card-id ::may-write-card]}
                          ::title-not-blank  {:doc      "The title is a non-blank string."
                                              :returned {:title " "}
                                              :blocked  [::title-not-the-card-id ::may-write-card]}}}
           (thrown-data #(pt/parse (->RenameCardDraft 1 -1 " "))))))
  (testing "a failed external check is reported as :external"
    (is (= {:type         ::pt/invalid
            :rule         :parsed-types.rule/checks-failed
            :failed-kinds #{:external}
            :draft        "parsed_types.core_test.RenameCardDraft"
            :failures     {::may-write-card {:doc "User 1 may write the card.", :returned false}}}
           (thrown-data #(pt/parse (->RenameCardDraft 1 3 "New"))))))
  (testing "who is acting is a field: the same card, another user"
    (is (= #{:external} (:failed-kinds (thrown-data #(pt/parse (->RenameCardDraft 2 1 "New"))))))))

(pt/defparsed Written [a b c]
  (pure ::a-int "a is an integer." [a] (log! ::a-int) (int? a))
  (pure ::a-pos "a is positive." [a] (log! ::a-pos) (pos? a))
  (pure ::a-below-b "a is below b." [a b] (log! ::a-below-b) (< a b))
  (pure ::c-int "c is an integer." [c] (log! ::c-int) (int? c)))

(deftest pure-checks-run-in-the-order-written-test
  (testing "in the order written, not by how many fields each reads"
    (is (= [::a-int ::a-pos ::a-below-b ::c-int] (runs #(pt/parse (->WrittenDraft 1 2 3))))))
  (testing "a failed check holds every later pure check that shares a field with it, and no other"
    (is (= [::a-int ::c-int] (runs #(thrown-data (fn [] (pt/parse (->WrittenDraft :x 2 3)))))))
    (is (= {::a-int {:doc "a is an integer.", :returned false, :blocked [::a-pos ::a-below-b]}}
           (:failures (thrown-data #(pt/parse (->WrittenDraft :x 2 3)))))))
  (testing "a check written before one that reads a proper subset of its fields does not compile"
    (is (re-find #"the pure check :parsed-types.core-test/ab reads \[a b\], and is written before :parsed-types.core-test/a, which reads only \[a\]\..*Move :parsed-types.core-test/a before :parsed-types.core-test/ab\."
                 (try (binding [*ns* (the-ns 'parsed-types.core-test)]
                        (eval '(pt/defparsed Unordered [a b] (pure ::ab "d" [a b] true) (pure ::a "d" [a] true))))
                      ""
                      (catch Throwable e (ex-message (ex-cause e))))))))

(deftest a-check-that-reads-no-field-may-go-anywhere-test
  (testing "it shares no field, so it holds nothing and is not a proper subset to move first"
    (binding [*ns* (the-ns 'parsed-types.core-test)]
      (eval '(pt/defparsed Anywhere [a] (pure ::a-int "a is an integer." [a] (int? a)) (pure ::always "Always." [] true))))
    (is (some? (pt/parse ((resolve 'parsed-types.core-test/->AnywhereDraft) 1))))))

(deftest order-test
  (testing "shape checks first, then checks over several fields, then fetches, then external checks"
    (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
           (runs #(pt/parse (->RenameCardDraft 1 1 "New"))))))
  (testing "a failed pure check holds up every fetch and external check, even one that does not read its field"
    (is (= [::card-id-positive ::title-not-blank]
           (runs #(thrown-data (fn [] (pt/parse (->RenameCardDraft 1 -1 "New")))))))
    (is (= [::card-id-positive ::title-not-blank]
           (runs #(thrown-data (fn [] (pt/parse (->RenameCardDraft 1 1 " ")))))))))

(deftest assoc-test
  (let [draft (->RenameCardDraft 1 1 "New")]
    (testing "assoc builds a new draft"
      (let [renamed (assoc draft :title "Newer")]
        (is (= (->RenameCardDraft 1 1 "Newer") renamed))
        (is (= "New" (:title draft)))))
    (testing "assoc refuses a key that is not a field, and dissoc any key"
      (is (= ::pt/no-field (:type (thrown-data #(assoc draft :titel "x")))))
      (is (= ::pt/cannot-change (:type (thrown-data #(dissoc draft :title))))))))

(deftest draft-of-test
  (let [parsed (pt/parse (->RenameCardDraft 1 1 "New"))
        draft  (pt/draft-of parsed)]
    (testing "a parsed value goes back to a draft with the same fields"
      (is (= (->RenameCardDraft 1 1 "New") draft)))
    (testing "editing it and parsing again runs every check"
      (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
             (runs #(pt/parse (assoc draft :title "Newer"))))))
    (testing "draft-of refuses anything parse did not issue"
      (is (= ::pt/not-parsed (:type (thrown-data #(pt/draft-of (RenameCard. 1 1 "New" nil)))))))))

(deftest batching-test
  (testing "parse-all calls each fetch once, with each distinct tuple once"
    (let [drafts [(->RenameCardDraft 1 1 "a") (->RenameCardDraft 1 2 "b") (->RenameCardDraft 1 1 "c")]
          parsed (atom nil)]
      (is (= [[::card [[1] [2]]]]
             (filter vector? (runs #(reset! parsed (pt/parse-all drafts))))))
      (is (= 3 (count @parsed)))
      (is (every? #(pt/parsed? RenameCard %) @parsed))))
  (testing "parse-all throws once with every failure, by index, and every kind that failed"
    (is (= {:type         ::pt/invalid
            :rule         :parsed-types.rule/checks-failed
            :failed-kinds #{:pure :external}
            :failures    {1 {::may-write-card {:doc "User 1 may write the card.", :returned false}}
                          2 {::card-id-positive {:doc      "The card id is a positive integer."
                                                 :returned false
                                                 :blocked  [::title-not-the-card-id ::may-write-card]}}}}
           (thrown-data #(pt/parse-all [(->RenameCardDraft 1 1 "a")
                                        (->RenameCardDraft 1 3 "b")
                                        (->RenameCardDraft 1 -1 "c")]))))))

(deftest a-draft-must-be-a-draft-type-test
  (testing "anything but a draft of a type defparsed defined is refused, even one that implements DraftValue"
    (is (= ::pt/bad-input (:type (thrown-data #(pt/parse (reify parsed_types.core.DraftValue))))))
    (is (= ::pt/bad-input (:type (thrown-data #(pt/parse-all [(reify parsed_types.core.DraftValue)])))))))

(deftest parsed-refuses-anything-parse-did-not-issue-test
  (let [real (pt/parse (->RenameCardDraft 1 1 "New"))]
    (doseq [[label x] [["nil" nil]
                       ["a plain map" {:user-id 1, :card-id 3, :title "pwned"}]
                       ["the draft" (->RenameCardDraft 1 1 "New")]
                       ["a parsed type built with its constructor" (RenameCard. 1 3 "pwned" nil)]
                       ["a value equal to a parsed one, built by hand" (RenameCard. 1 1 "New" nil)]
                       ["a value built with a proof of its own" (RenameCard. 1 1 "New" (Object.))]
                       ["a parsed value of another type" (pt/parse (->OtherDraft 1 3 "pwned"))]
                       ["an edn round trip" (edn/read-string {:default (fn [_ m] m)} (pr-str real))]]]
      (testing label
        (is (= {:type ::pt/not-parsed, :expected "parsed_types.core_test.RenameCard", :got (some-> x class .getName)}
               (dissoc (thrown-data #(pt/parsed RenameCard x)) :rule)))
        (is (false? (pt/parsed? RenameCard x)))))
    (is (true? (pt/parsed? RenameCard real)))))

(deftest the-proof-is-not-a-key-test
  (let [real (pt/parse (->RenameCardDraft 1 1 "New"))]
    (testing "the hidden parse-proof field is not part of the map"
      (is (= [:user-id :card-id :title] (keys real)))
      (is (= 3 (count real) (count (seq real)) (.size ^java.util.Map real)))
      (is (false? (contains? real :parse-proof)))
      (is (= ::none (get real :parse-proof ::none)))
      (is (= ::pt/no-field (:type (thrown-data #(:parse-proof real))))))
    (testing "equality holds the proof: a value parse issued never equals one built without it"
      (is (not= real (RenameCard. 1 1 "New" nil)))
      (is (not= (RenameCard. 1 1 "New" nil) real))
      (is (not (.equals ^Object real (RenameCard. 1 1 "New" nil))))
      (is (false? (contains? #{real} (RenameCard. 1 1 "New" nil))))
      (is (= real (pt/parse (->RenameCardDraft 1 1 "New"))) "two parsed values with equal fields are equal")
      (is (= (RenameCard. 1 1 "New" nil) (RenameCard. 1 1 "New" nil)) "and so are two unproven ones"))
    (testing "hash and print read only the fields"
      (is (= (hash real) (hash (RenameCard. 1 1 "New" nil))))
      (is (= (pr-str real) (pr-str (RenameCard. 1 1 "New" nil))))
      (is (not (re-find #"proof" (pr-str real)))))))

;; Asks to see the proof, and keeps what it is shown.
(def ^:private stolen (atom nil))

(deftype Thief []
  parsed_types.core.ParsedValue
  (issuedWith [_ t] (reset! stolen t) true))

(deftest the-proof-never-leaves-the-value-test
  (let [real (pt/parse (->RenameCardDraft 1 1 "New"))]
    (testing "the proof field is not public, so interop cannot read it, and no public method returns it"
      (is (not (java.lang.reflect.Modifier/isPublic (.getModifiers (.getDeclaredField RenameCard "parse_proof")))))
      (is (thrown? IllegalArgumentException (clojure.lang.Reflector/getInstanceField real "parse_proof")))
      (is (not-any? #(and (zero? (.getParameterCount ^java.lang.reflect.Method %))
                          (= Object (.getReturnType ^java.lang.reflect.Method %))
                          ;; IFn's call and invoke with no arguments throw an arity error
                          (not (#{"call" "invoke"} (.getName ^java.lang.reflect.Method %))))
                    (.getMethods RenameCard))))
    (testing "a hand-written ParsedValue is refused without being shown the proof"
      (reset! stolen nil)
      (is (false? (pt/parsed? RenameCard (->Thief))))
      (is (= ::pt/not-parsed (:type (thrown-data #(pt/draft-of (->Thief))))))
      (is (nil? @stolen)))))

(deftest the-positional-factory-is-removed-test
  (testing "->Name of a parsed type does not exist, here or anywhere; ->NameDraft does"
    (is (nil? (ns-resolve 'parsed-types.core-test '->RenameCard)))
    (is (some? (ns-resolve 'parsed-types.core-test '->RenameCardDraft)))
    (is (re-find #"Unable to resolve symbol: ->RenameCard"
                 (try (binding [*ns* (the-ns 'parsed-types.core-test)] (eval '(->RenameCard 1 1 "New"))) nil
                      (catch Throwable e (ex-message (or (ex-cause e) e))))))))

(deftest a-parsed-value-cannot-be-changed-test
  (let [real (pt/parse (->RenameCardDraft 1 1 "New"))]
    (doseq [[label thunk] [["assoc" #(assoc real :card-id 3)]
                           ["dissoc" #(dissoc real :card-id)]
                           ["merge" #(merge real {:card-id 3})]
                           ["update" #(update real :card-id inc)]
                           ["with-meta" #(with-meta real {:x 1})]
                           ["vary-meta" #(vary-meta real assoc :x 1)]]]
      (testing label
        (is (= ::pt/cannot-change (:type (thrown-data thunk))))))
    (testing "the draft type is a record, and the parse is still the only way to the parsed one"
      (is (= ::pt/cannot-change (:type (thrown-data #(assoc real :card-id 3))))))))

;; Claims to be a draft, and answers `get` with a value its field does not hold.
(deftype LyingDraft [a]
  parsed_types.core.DraftValue
  clojure.lang.ILookup
  (valAt [_ k] (when (= :a k) -999))
  (valAt [_ k not-found] (if (= :a k) -999 not-found)))

(deftest a-hand-made-draft-type-is-refused-test
  (testing "a deftype written by hand has no plan, so it parses to nothing"
    (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/not-a-draft, :got "parsed_types.core_test.LyingDraft"}
           (thrown-data #(pt/parse (->LyingDraft 5)))))))

(pt/defparsed Closure [a]
  (external ::known "a is in the db." [] [row ::row] (some? row))
  (fetch ::row [a] (let [db {1 :one}] (fn [tuples] (into {} (map (fn [[id :as t]] [t (get db id)])) tuples)))))

(deftest a-fetch-may-close-over-a-local-test
  (testing "a fetch reads the world, so it may hold a database handle"
    (is (= {:a 1} (into {} (pt/parse (->ClosureDraft 1)))))
    (is (= #{:external} (:failed-kinds (thrown-data #(pt/parse (->ClosureDraft 2))))))))

(deftest empty-throws-test
  (let [real  (pt/parse (->RenameCardDraft 1 1 "New"))
        draft (->RenameCardDraft 1 1 "New")]
    (testing "a parsed value and a draft have no empty version"
      (doseq [x [real draft]]
        (is (= {:type ::pt/cannot-change, :op "empty"}
               (select-keys (thrown-data #(empty x)) [:type :op])))))
    (testing "so a walk over a structure holding a parsed value throws, never drops the proof"
      (is (= ::pt/cannot-change (:type (thrown-data #(walk/postwalk identity {:rename [real]})))))
      (is (= ::pt/cannot-change (:type (thrown-data #(walk/prewalk identity [real]))))))))

(defrecord Point [x y])

(defn- refusal [v]
  (select-keys (thrown-data #(pt/parse (->OtherDraft 1 1 v))) [:type :field :path :got]))

(deftest mutable-field-values-are-refused-test
  (testing "a field must hold a value known to be immutable, all the way down; anything else is bad input"
    (doseq [[label v path got] [["an array" (int-array [1]) [] "[I"]
                                ["an atom" (atom 1) [] "clojure.lang.Atom"]
                                ["a ref" (ref 1) [] "clojure.lang.Ref"]
                                ["a volatile" (volatile! 1) [] "clojure.lang.Volatile"]
                                ["an atomic value" (java.util.concurrent.atomic.AtomicLong. 1) []
                                 "java.util.concurrent.atomic.AtomicLong"]
                                ["a java.util.Date" (java.util.Date.) [] "java.util.Date"]
                                ["a Java list" (java.util.ArrayList. [1]) [] "java.util.ArrayList"]
                                ["a Java map" (java.util.HashMap. {:a 1}) [] "java.util.HashMap"]
                                ["a lazy seq" (map inc [1 2]) [] "clojure.lang.LazySeq"]
                                ["a seq" (seq [1 2]) [] "clojure.lang.PersistentVector$ChunkedSeq"]
                                ["a transient" (transient []) [] "clojure.lang.PersistentVector$TransientVector"]
                                ["an atom in a vector in a map" {:a [1 (atom 1)]} [:a 1] "clojure.lang.Atom"]
                                ["an atom as a map key" {(atom 1) 1} '[<key>] "clojure.lang.Atom"]
                                ["an array in a set in a record" (->Point #{(int-array 1)} 2) '[:x <element>] "[I"]]]
      (testing label
        (is (= {:type ::pt/bad-input, :field :title, :path path, :got got} (refusal v))))))
  (testing "no check runs"
    (is (= [] (runs #(thrown-data (fn [] (pt/parse (->RenameCardDraft 1 1 (atom "New")))))))))
  (testing "the message names the field, the path in it and the fix"
    (doseq [[v message]
            [[{:a [1 (atom 1)]}
              (str "OtherDraft: the field :title at [:a 1] holds an atom (clojure.lang.Atom). A parsed value holds only"
                   " values known to be immutable, so what its checks proved stays true. Deref it, as in @a.")]
             [(java.util.Date. 0)
              (str "OtherDraft: the field :title holds a mutable date (java.util.Date). A parsed value"
                   " holds only values known to be immutable, so what its checks proved stays true. Use"
                   " java.time.Instant, as in (.toInstant d).")]
             [(map inc [1])
              (str "OtherDraft: the field :title holds a lazy seq, which realizing could run side effects or never end"
                   " (clojure.lang.LazySeq). A parsed value holds only values known to be immutable, so what its checks"
                   " proved stays true. Pass a vector, as in (vec s).")]
             [(int-array 1)
              (str "OtherDraft: the field :title holds a Java array ([I). A parsed value holds only values known to be"
                   " immutable, so what its checks proved stays true. Copy it into a vector, as in (into [] a).")]]]
      (is (= message (try (pt/parse (->OtherDraft 1 1 v)) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
  (testing "immutable values pass: scalars, java.time values, persistent collections, records and parsed values"
    (doseq [v [nil true \a "s" :k 'sym 1 (int 1) 1.5 1M 1N 1/3 (biginteger 1) (random-uuid) #"re" String
               (java.time.Instant/now) (java.time.LocalDate/of 2026 1 1) (java.time.ZoneId/of "Europe/Paris")
               (java.time.Duration/ofSeconds 1) java.time.DayOfWeek/MONDAY
               [1 [2 #{3}] '(4 5) {:a {:b (java.time.Instant/now)}}] (sorted-map :a 1) clojure.lang.PersistentQueue/EMPTY
               (->Point 1 [2]) (pt/parse (->OtherDraft 1 1 nil))]]
      (is (some? (pt/parse (->OtherDraft 1 1 v))) (pr-str v)))))

(deftest the-array-fix-copies-the-array-test
  (testing "the fix the message names makes a value the array cannot change"
    (let [a     (object-array [1 2])
          fixed (into [] a)
          p     (pt/parse (->OtherDraft 1 1 fixed))]
      (aset a 0 :mutated)
      (is (= [1 2] (:title p)))))
  (testing "while vec, which the message no longer names, shares a small array"
    (let [a (object-array [1 2]), v (vec a)]
      (aset a 0 :mutated)
      (is (= [:mutated 2] v)))))

(deftest metadata-and-comparators-are-walked-test
  (testing "metadata is walked like the value it is on"
    (is (= {:type ::pt/bad-input, :field :title, :path '[<meta> :a], :got "clojure.lang.Atom"}
           (refusal (with-meta [1] {:a (atom 1)}))))
    (is (= '[:k <meta> :a] (:path (refusal {:k (with-meta 'sym {:a (atom 1)})}))))
    (is (some? (pt/parse (->OtherDraft 1 1 (with-meta [1] {:doc "fine"}))))))
  (testing "a sorted collection must sort with the default comparator"
    (doseq [v [(sorted-set-by (fn [a b] (compare a b)) 1 2) (sorted-map-by > 1 :a)]]
      (is (= ::pt/bad-input (:type (refusal v))))
      (is (re-find #"custom comparator.*Use \(sorted-map \.\.\.\) or \(sorted-set \.\.\.\)"
                   (try (pt/parse (->OtherDraft 1 1 v)) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))))
    (is (some? (pt/parse (->OtherDraft 1 1 [(sorted-set 1 2) (sorted-map :a 1)]))))))

(deftest shared-structure-is-walked-once-test
  (testing "each collection is walked once per parse, so a value with 2^40 paths to its leaf parses fast"
    (let [shared (reduce (fn [x _] [x x]) 1 (range 40))
          result (promise)
          ;; a daemon thread, so a walk that does not finish fails this test and does not hold the JVM open
          _      (doto (Thread. ^Runnable #(deliver result (some? (pt/parse (->OtherDraft 1 1 shared)))))
                   (.setDaemon true)
                   (.start))]
      (is (true? (deref result 1000 ::timed-out)))))
  (testing "a mutable value under shared structure is still found"
    (let [shared (reduce (fn [x _] [x x]) (atom 1) (range 40))]
      (is (= "clojure.lang.Atom" (:got (refusal shared)))))))

(defn- on-small-stack
  "What `f` returns on a thread with a 256 KB stack, or the class of what it threw."
  [f]
  (let [p (promise)]
    (doto (Thread. nil #(deliver p (try (f) (catch Throwable t (class t)))) "small-stack" (* 256 1024))
      (.setDaemon true)
      (.start))
    (deref p 10000 ::timed-out)))

(deftest the-walk-keeps-its-own-stack-test
  (testing "values nested to the limit parse on a small stack, and deeper ones are bad input there too"
    (doseq [nest [(fn [n] (reduce (fn [x _] [x]) :x (range n)))
                  (fn [n] (reduce (fn [x _] {:k x}) :x (range n)))
                  (fn [n] (reduce (fn [x _] #{x}) :x (range n)))
                  (fn [n] (reduce (fn [x _] {x 1}) :x (range n)))
                  (fn [n] (reduce (fn [x _] (with-meta [x] {:m [1]})) :x (range n)))]]
      (is (true? (on-small-stack #(some? (pt/parse (->OtherDraft 1 1 (nest 990)))))))
      (is (= ::pt/bad-input (on-small-stack #(:type (refusal (nest 1200)))))))))

(deftest shared-structure-is-walked-again-when-reached-deeper-test
  (testing "a collection already walked near the top is walked again when it is reached deeper, so the limit holds"
    (let [inner (reduce (fn [x _] [x]) :x (range 900))
          outer (reduce (fn [x _] [x]) inner (range 300))]
      (is (= ::pt/bad-input (:type (refusal [inner outer]))))
      (is (= ::pt/bad-input (:type (refusal [outer inner]))))
      (is (some? (pt/parse (->OtherDraft 1 1 [inner inner]))))))
  (testing "and a mutable value it holds is found on either path"
    (let [inner [(atom 1)]]
      (is (= [1 0] (:path (refusal [[] inner [inner]])))))))

(deftest a-clock-is-refused-test
  (testing "a clock reads the time each time it is asked, though its class is a final java.time class"
    (doseq [clock [(java.time.Clock/systemUTC) (java.time.Clock/fixed java.time.Instant/EPOCH java.time.ZoneOffset/UTC)
                   (java.time.Clock/offset (java.time.Clock/systemUTC) (java.time.Duration/ofSeconds 1))]]
      (is (= ::pt/bad-input (:type (refusal clock))) (str (class clock)))
      (is (re-find #"holds a clock, which reads the time each time it is asked.*Pass the instant it reads, as in \(\.instant clock\)\."
                   (try (pt/parse (->OtherDraft 1 1 clock)) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
  (testing "the instant it reads is fine"
    (is (some? (pt/parse (->OtherDraft 1 1 (.instant (java.time.Clock/systemUTC))))))))

(deftest a-deep-path-is-truncated-in-the-message-test
  (let [nested (reduce (fn [x _] [x]) (atom 1) (range 20))
        msg    (try (pt/parse (->OtherDraft 1 1 nested)) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (re-find #"at \[0 0 0 0 0 0 0 0\] and 12 levels more holds an atom" msg))
    (is (= 20 (count (:path (refusal nested)))) "the ex-data keeps the whole path")))

(defn- on-daemon
  "What `f` returns on a daemon thread within `ms`, or ::timed-out, so a walk that does not finish fails the test and
  does not hold the JVM open."
  [ms f]
  (let [p (promise)]
    (doto (Thread. ^Runnable #(deliver p (try (f) (catch Throwable t t))))
      (.setDaemon true)
      (.start))
    (deref p ms ::timed-out)))

(deftest a-collection-reached-at-many-depths-is-walked-once-test
  (testing "a large collection reached at every level of a deep chain costs its size once, not once per level"
    (let [shared (vec (range 100000))
          chain  (reduce (fn [acc _] [shared acc]) [] (range 990))]
      (is (true? (on-daemon 1000 #(some? (pt/parse (->OtherDraft 1 1 chain))))))))
  (testing "and one whose deepest value is past the limit from where it is reached again is refused there"
    (let [inner (reduce (fn [x _] [x]) :x (range 600))
          chain (reduce (fn [acc _] [inner acc]) [] (range 500))]
      (is (= :parsed-types.rule/value-too-deep (:rule (thrown-data #(pt/parse (->OtherDraft 1 1 chain)))))))))

(deftest a-value-too-deep-test
  (let [nested (reduce (fn [x _] [x]) 1 (range 1001))
        data   (thrown-data #(pt/parse (->OtherDraft 1 1 nested)))]
    (testing "is its own rule, with its path cut to the first steps, and its depth"
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/value-too-deep, :field :title, :path [0 0 0 0 0 0 0 0]
              :depth 1001}
             (select-keys data [:type :rule :field :path :depth]))))
    (testing "and each fix the message names parses"
      (is (= [1] (:title (pt/parse (->OtherDraft 1 1 (vec (flatten nested)))))))
      (is (string? (:title (pt/parse (->OtherDraft 1 1 (pr-str nested)))))))))

(deftest java-sql-dates-test
  (testing "java.sql.Date and java.sql.Time throw on .toInstant, so each message names a conversion that works"
    (doseq [[v fix convert] [[(java.sql.Date. 0) "(.toLocalDate d)" #(.toLocalDate ^java.sql.Date %)]
                             [(java.sql.Time. 0) "(.toLocalTime d)" #(.toLocalTime ^java.sql.Time %)]
                             [(java.sql.Timestamp. 0) "(.toInstant d)" #(.toInstant ^java.sql.Timestamp %)]]]
      (let [message (try (pt/parse (->OtherDraft 1 1 v)) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (str/includes? (str message) fix) (str (class v)))
        (is (some? (pt/parse (->OtherDraft 1 1 (convert v)))) (str (class v)))))))

(pt/defparsed Line [n] (pure ::n-int "n is an integer." [n] (int? n)))

(pt/defparsed Entry [lines x y]
  (children lines Line)
  (pure ::x-int "x is an integer." [x] (int? x))
  (pure ::y-int "y is an integer." [y] (int? y))
  (pure ::sum-under-x "The lines add up to less than x." [lines x] (< (reduce + (map :n lines)) x))
  (pure ::x-under-y "x is below y." [x y] (< x y)))

(deftest a-held-check-holds-nothing-test
  (testing "a check held by a failed child did not run, so a later check that shares a field with it still runs"
    (is (= {:lines        {0 {::n-int {:doc "n is an integer.", :returned false}}}
            ::sum-under-x {:doc "The lines add up to less than x.", :blocked-by [[:lines 0]]}
            ::x-under-y   {:doc "x is below y.", :returned false}}
           (:failures (thrown-data #(pt/parse (->EntryDraft [(->LineDraft :bad)] 5 1)))))))
  (testing "while a check that ran and failed holds it"
    (is (= {::sum-under-x {:doc "The lines add up to less than x.", :returned false, :blocked [::x-under-y]}}
           (:failures (thrown-data #(pt/parse (->EntryDraft [(->LineDraft 100)] 5 1))))))))
