(ns parsed-types.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.pprint :as pprint]
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
    (let [input  {:user-id 1, :card-id 1, :title "New"}
          parsed (pt/parse RenameCard input)]
      (is (instance? RenameCard parsed))
      (is (= input (pt/parsed RenameCard parsed)))
      (is (false? (pt/parsed? RenameCard input)) "the input map is not the parsed value")
      (testing "it reads only through pt/parsed, and compares like a record"
        (is (not= {:user-id 1, :card-id 1, :title "New"} parsed) "a parsed value never equals a plain map")
        (is (= parsed (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})) "it equals another parsed value with the same fields")
        (is (not= parsed (RenameCard. 1 1 "New" nil)) "and never a value of its type built without the proof")
        (is (= 1 (.-card-id ^RenameCard parsed)))
        (is (= ::pt/must-unwrap (:type (thrown-data #(:card-id parsed)))) "a keyword read throws, for a field")
        (is (= ::pt/must-unwrap (:type (thrown-data #(get parsed :nope ::none)))) "and for any other key, with a default too"))
      (testing "it prints with its type, and so do str, toString and pprint, which never show the proof"
        (doseq [shown [(pr-str parsed) (str parsed) (.toString ^Object parsed) (str/trim (with-out-str (binding [pprint/*print-right-margin* 200] (pprint/pprint parsed))))]]
          (is (= "#parsed_types.core_test.RenameCard{:user-id 1, :card-id 1, :title \"New\"}" shown)))
        (is (= "{:k [#parsed_types.core_test.RenameCard{:user-id 1, :card-id 1, :title \"New\"}]}"
               (str/trim (with-out-str (binding [pprint/*print-right-margin* 200] (pprint/pprint {:k [parsed]})))))))
      (testing "by one method on the library's interfaces: a defparsed adds no print method"
        (let [before (count (methods print-method))]
          (binding [*ns* (the-ns 'parsed-types.core-test)] (eval '(pt/defparsed Printed [a])))
          (is (= before (count (methods print-method))))
          (is (not (contains? (methods print-method) RenameCard)))))))
  (testing "the first failed check stops the parse, and is reported by name, with its doc and what it returned"
    (is (= {:type     ::pt/invalid
            :rule     :parsed-types.rule/check-failed
            :check    ::card-id-positive
            :doc      "The card id is a positive integer."
            :kind     :pure
            :returned false
            :parsed   "parsed_types.core_test.RenameCard"}
           (thrown-data #(pt/parse RenameCard {:user-id 1, :card-id -1, :title " "}))))
    (is (= [::card-id-positive] (runs #(thrown-data (fn [] (pt/parse RenameCard {:user-id 1, :card-id -1, :title " "})))))
        "no check after it runs")
    (is (= {:title " "} (:returned (thrown-data #(pt/parse RenameCard {:user-id 1, :card-id 1, :title " "}))))
        "what a check returned, when it says why"))
  (testing "a failed external check is reported as :external"
    (is (= {:type     ::pt/invalid
            :rule     :parsed-types.rule/check-failed
            :check    ::may-write-card
            :doc      "User 1 may write the card."
            :kind     :external
            :returned false
            :parsed   "parsed_types.core_test.RenameCard"}
           (thrown-data #(pt/parse RenameCard {:user-id 1, :card-id 3, :title "New"})))))
  (testing "who is acting is a field: the same card, another user"
    (is (= :external (:kind (thrown-data #(pt/parse RenameCard {:user-id 2, :card-id 1, :title "New"})))))))

(pt/defparsed Written [a b c]
  (pure ::a-int "a is an integer." [a] (log! ::a-int) (int? a))
  (pure ::a-pos "a is positive." [a] (log! ::a-pos) (pos? a))
  (pure ::a-below-b "a is below b." [a b] (log! ::a-below-b) (< a b))
  (pure ::c-int "c is an integer." [c] (log! ::c-int) (int? c)))

(deftest pure-checks-run-in-the-order-written-test
  (testing "in the order written, not by how many fields each reads"
    (is (= [::a-int ::a-pos ::a-below-b ::c-int] (runs #(pt/parse Written {:a 1, :b 2, :c 3})))))
  (testing "the first failed check stops the parse: no later check runs, whatever fields it reads"
    (is (= [::a-int] (runs #(thrown-data (fn [] (pt/parse Written {:a :x, :b 2, :c 3}))))))
    (is (= ::a-int (:check (thrown-data #(pt/parse Written {:a :x, :b 2, :c :y}))))))
  (testing "a check written before one that reads a proper subset of its fields does not compile"
    (is (re-find #"the pure check :parsed-types.core-test/ab reads \[a b\], and is written before :parsed-types.core-test/a, which reads only \[a\]\..*Move :parsed-types.core-test/a before :parsed-types.core-test/ab\."
                 (try (binding [*ns* (the-ns 'parsed-types.core-test)]
                        (eval '(pt/defparsed Unordered [a b] (pure ::ab "d" [a b] true) (pure ::a "d" [a] true))))
                      ""
                      (catch Throwable e (ex-message (ex-cause e))))))))

(deftest a-check-that-reads-no-field-may-go-anywhere-test
  (testing "it reads no field, so it is not a proper subset to move first"
    (binding [*ns* (the-ns 'parsed-types.core-test)]
      (eval '(pt/defparsed Anywhere [a] (pure ::a-int "a is an integer." [a] (int? a)) (pure ::always "Always." [] true))))
    (is (some? (binding [*ns* (the-ns 'parsed-types.core-test)] (eval '(pt/parse Anywhere {:a 1})))))))

(deftest order-test
  (testing "pure checks in the order written, then fetches, then external checks"
    (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
           (runs #(pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})))))
  (testing "a failed pure check stops the parse before any fetch or external check"
    (is (= [::card-id-positive]
           (runs #(thrown-data (fn [] (pt/parse RenameCard {:user-id 1, :card-id -1, :title "New"}))))))
    (is (= [::card-id-positive ::title-not-blank]
           (runs #(thrown-data (fn [] (pt/parse RenameCard {:user-id 1, :card-id 1, :title " "}))))))))

(deftest a-changed-copy-is-a-changed-map-parsed-again-test
  (let [parsed (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})]
    (testing "pt/parsed returns the fields, which parse back to an equal value"
      (is (= parsed (pt/parse RenameCard (pt/parsed RenameCard parsed)))))
    (testing "changing one and parsing again runs every check"
      (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
             (runs #(pt/parse RenameCard (assoc (pt/parsed RenameCard parsed) :title "Newer"))))))
    (testing "assoc on a parsed value refuses a key that is not a field, naming the fields"
      (is (= {:type ::pt/no-field, :key :titel, :suggestion :title}
             (select-keys (thrown-data #(assoc parsed :titel "x")) [:type :key :suggestion]))))))

(deftest batching-test
  (testing "parse-all calls each fetch once, with each distinct tuple once"
    (let [ms     [{:user-id 1, :card-id 1, :title "a"} {:user-id 1, :card-id 2, :title "b"} {:user-id 1, :card-id 1, :title "c"}]
          parsed (atom nil)]
      (is (= [[::card [[1] [2]]]]
             (filter vector? (runs #(reset! parsed (pt/parse-all RenameCard ms))))))
      (is (= 3 (count @parsed)))
      (is (every? #(pt/parsed? RenameCard %) @parsed))))
  (testing "parse-all runs every map's pure checks before any fetch, and throws for the first check that fails"
    (is (= {:type     ::pt/invalid
            :rule     :parsed-types.rule/check-failed
            :check    ::card-id-positive
            :doc      "The card id is a positive integer."
            :kind     :pure
            :returned false
            :parsed   "parsed_types.core_test.RenameCard"
            :index    2}
           (thrown-data #(pt/parse-all RenameCard [{:user-id 1, :card-id 1, :title "a"}
                                                   {:user-id 1, :card-id 3, :title "b"}
                                                   {:user-id 1, :card-id -1, :title "c"}]))))
    (is (not-any? vector? (runs #(thrown-data (fn [] (pt/parse-all RenameCard [{:user-id 1, :card-id 3, :title "b"}
                                                                               {:user-id 1, :card-id -1, :title "c"}])))))
        "so a pure failure in a later map fetches nothing"))
  (testing "when every pure check passes, the first external failure names its map"
    (is (= {:check ::may-write-card, :index 1}
           (select-keys (thrown-data #(pt/parse-all RenameCard [{:user-id 1, :card-id 1, :title "a"}
                                                                {:user-id 1, :card-id 3, :title "b"}]))
                        [:check :index])))))

(deftest parse-takes-a-parsed-type-test
  (testing "anything but a type defparsed defined is refused, even a class that implements ParsedValue"
    (doseq [t [(class (reify parsed_types.core.ParsedValue (issuedWith [_ _] false))) String :RenameCard nil]]
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/not-a-parsed-type}
             (select-keys (thrown-data #(pt/parse t {:a 1})) [:type :rule])) (pr-str t))
      (is (= ::pt/bad-input (:type (thrown-data #(pt/parse-all t [{:a 1}])))) (pr-str t)))))

(deftest parsed-refuses-anything-parse-did-not-issue-test
  (let [real (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})]
    (doseq [[label x] [["nil" nil]
                       ["a plain map" {:user-id 1, :card-id 3, :title "pwned"}]
                       ["a parsed type built with its constructor" (RenameCard. 1 3 "pwned" nil)]
                       ["a value equal to a parsed one, built by hand" (RenameCard. 1 1 "New" nil)]
                       ["a value built with a proof of its own" (RenameCard. 1 1 "New" (Object.))]
                       ["a parsed value of another type" (pt/parse Other {:user-id 1, :card-id 3, :title "pwned"})]
                       ["an edn round trip" (edn/read-string {:default (fn [_ m] m)} (pr-str real))]]]
      (testing label
        (is (= {:type ::pt/not-parsed, :expected "parsed_types.core_test.RenameCard", :got (some-> x class .getName)}
               (dissoc (thrown-data #(pt/parsed RenameCard x)) :rule)))
        (is (false? (pt/parsed? RenameCard x)))))
    (is (true? (pt/parsed? RenameCard real)))))

(deftest the-proof-is-not-a-key-test
  (let [real (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})]
    (testing "the hidden parse-proof field is not a field: pt/parsed returns only the fields"
      (is (= [:user-id :card-id :title] (keys (pt/parsed RenameCard real))))
      (is (= ::pt/must-unwrap (:type (thrown-data #(:parse-proof real))))))
    (testing "equality holds the proof: a value parse issued never equals one built without it"
      (is (not= real (RenameCard. 1 1 "New" nil)))
      (is (not= (RenameCard. 1 1 "New" nil) real))
      (is (not (.equals ^Object real (RenameCard. 1 1 "New" nil))))
      (is (false? (contains? #{real} (RenameCard. 1 1 "New" nil))))
      (is (= real (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})) "two parsed values with equal fields are equal")
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
  (let [real (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})]
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
      (is (= ::pt/not-parsed (:type (thrown-data #(pt/parsed RenameCard (->Thief))))))
      (is (nil? @stolen)))))

(deftest the-positional-factories-are-removed-test
  (testing "->Name does not exist: pt/parse names every field, so reordering fields swaps nothing"
    (doseq [sym '[->RenameCard]]
      (is (nil? (ns-resolve 'parsed-types.core-test sym)))
      (is (re-find (re-pattern (str "Unable to resolve symbol: " sym))
                   (try (binding [*ns* (the-ns 'parsed-types.core-test)] (eval (list sym 1 1 "New"))) nil
                        (catch Throwable e (ex-message (or (ex-cause e) e)))))))))

(deftest a-parsed-value-cannot-be-changed-test
  (let [real (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})]
    (doseq [[label thunk] [["assoc" #(assoc real :card-id 3)]
                           ["merge" #(merge real {:card-id 3})]
                           ["with-meta" #(with-meta real {:x 1})]
                           ["vary-meta" #(vary-meta real assoc :x 1)]]]
      (testing label
        (is (= ::pt/cannot-change (:type (thrown-data thunk))))))
    (testing "update reads the field first, so it throws ::must-unwrap"
      (is (= ::pt/must-unwrap (:type (thrown-data #(update real :card-id inc))))))
    (testing "dissoc: a parsed value is not a map"
      (is (thrown? ClassCastException (dissoc real :card-id))))))

(pt/defparsed Closure [a]
  (external ::known "a is in the db." [] [row ::row] (some? row))
  (fetch ::row [a] (let [db {1 :one}] (fn [tuples] (into {} (map (fn [[id :as t]] [t (get db id)])) tuples)))))

(deftest a-fetch-may-close-over-a-local-test
  (testing "a fetch reads the world, so it may hold a database handle"
    (is (= {:a 1} (pt/parsed Closure (pt/parse Closure {:a 1}))))
    (is (= :external (:kind (thrown-data #(pt/parse Closure {:a 2})))))))

(defn- rows-by-id [tuples] (into {} (map (fn [t] [t {:id (first t)}])) tuples))

;; public: only the quoted form below names it
(def not-a-fn 42)

(deftest a-fetch-may-be-a-var-test
  (binding [*ns* (the-ns 'parsed-types.core-test)]
    (eval '(pt/defparsed ByVar [id]
             (external ::found "The row exists." [] [row ::row] (some? row))
             (fetch ::row [id] #'rows-by-id))))
  (let [parse-one #(binding [*ns* (the-ns 'parsed-types.core-test)] (eval '(pt/parse ByVar {:id 1})))]
    (testing "the var is called through, so with-redefs stubs the fetch"
      (is (some? (parse-one)))
      (with-redefs [rows-by-id (fn [tuples] (into {} (map (fn [t] [t nil])) tuples))]
        (is (= :external (:kind (thrown-data parse-one)))))))
  (testing "a var that holds anything but a fn is refused when the defparsed loads"
    (let [e (try (binding [*ns* (the-ns 'parsed-types.core-test)]
                   (eval '(pt/defparsed ByNumber [id] (fetch ::row [id] #'not-a-fn))))
                 nil
                 (catch Exception e e))]
      (is (= :parsed-types.rule/fetch-not-fn (:rule (ex-data (or (ex-cause e) e))))))))

(deftest empty-throws-test
  (let [real (pt/parse RenameCard {:user-id 1, :card-id 1, :title "New"})]
    (testing "a parsed value has no empty version"
      (is (= {:type ::pt/cannot-change, :op "empty"}
             (select-keys (thrown-data #(empty real)) [:type :op]))))
    (testing "so a walk over a structure holding a parsed value throws, never drops the proof"
      (is (= ::pt/cannot-change (:type (thrown-data #(walk/postwalk identity {:rename [real]})))))
      (is (= ::pt/cannot-change (:type (thrown-data #(walk/prewalk identity [real]))))))))

(defrecord Point [x y])

(defn- refusal [v]
  (select-keys (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title v})) [:type :field :path :got]))

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
    (is (= [] (runs #(thrown-data (fn [] (pt/parse RenameCard {:user-id 1, :card-id 1, :title (atom "New")})))))))
  (testing "the message names the field, the path in it and the fix"
    (doseq [[v message]
            [[{:a [1 (atom 1)]}
              (str "Other: the field :title at [:a 1] holds an atom (clojure.lang.Atom). A parsed value holds only"
                   " values known to be immutable, so what its checks proved stays true. Deref it, as in @a.")]
             [(java.util.Date. 0)
              (str "Other: the field :title holds a mutable date (java.util.Date). A parsed value"
                   " holds only values known to be immutable, so what its checks proved stays true. Use"
                   " java.time.Instant, as in (.toInstant d).")]
             [(map inc [1])
              (str "Other: the field :title holds a lazy seq, which realizing could run side effects or never end"
                   " (clojure.lang.LazySeq). A parsed value holds only values known to be immutable, so what its checks"
                   " proved stays true. Pass a vector, as in (vec s).")]
             [(int-array 1)
              (str "Other: the field :title holds a Java array ([I). A parsed value holds only values known to be"
                   " immutable, so what its checks proved stays true. Copy it into a vector, as in (into [] a).")]]]
      (is (= message (try (pt/parse Other {:user-id 1, :card-id 1, :title v}) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
  (testing "immutable values pass: scalars, java.time values, persistent collections, records and parsed values"
    (doseq [v [nil true \a "s" :k 'sym 1 (int 1) 1.5 1M 1N 1/3 (biginteger 1) (random-uuid) #"re" String
               (java.time.Instant/now) (java.time.LocalDate/of 2026 1 1) (java.time.ZoneId/of "Europe/Paris")
               (java.time.Duration/ofSeconds 1) java.time.DayOfWeek/MONDAY
               [1 [2 #{3}] '(4 5) {:a {:b (java.time.Instant/now)}}] (sorted-map :a 1) clojure.lang.PersistentQueue/EMPTY
               (->Point 1 [2]) (pt/parse Other {:user-id 1, :card-id 1, :title nil})]]
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title v})) (pr-str v)))))

(deftest the-array-fix-copies-the-array-test
  (testing "the fix the message names makes a value the array cannot change"
    (let [a     (object-array [1 2])
          fixed (into [] a)
          p     (pt/parse Other {:user-id 1, :card-id 1, :title fixed})]
      (aset a 0 :mutated)
      (is (= [1 2] (:title (pt/parsed Other p))))))
  (testing "while vec, which the message no longer names, shares a small array"
    (let [a (object-array [1 2]), v (vec a)]
      (aset a 0 :mutated)
      (is (= [:mutated 2] v)))))

(deftest metadata-and-comparators-are-walked-test
  (testing "metadata is walked like the value it is on"
    (is (= {:type ::pt/bad-input, :field :title, :path '[<meta> :a], :got "clojure.lang.Atom"}
           (refusal (with-meta [1] {:a (atom 1)}))))
    (is (= '[:k <meta> :a] (:path (refusal {:k (with-meta 'sym {:a (atom 1)})}))))
    (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title (with-meta [1] {:doc "fine"})}))))
  (testing "a sorted collection must sort with the default comparator"
    (doseq [v [(sorted-set-by (fn [a b] (compare a b)) 1 2) (sorted-map-by > 1 :a)]]
      (is (= ::pt/bad-input (:type (refusal v))))
      (is (re-find #"custom comparator.*Use \(sorted-map \.\.\.\) or \(sorted-set \.\.\.\)"
                   (try (pt/parse Other {:user-id 1, :card-id 1, :title v}) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))))
    (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title [(sorted-set 1 2) (sorted-map :a 1)]})))))

(deftest shared-structure-is-walked-once-test
  (testing "each collection is walked once per parse, so a value with 2^40 paths to its leaf parses fast"
    (let [shared (reduce (fn [x _] [x x]) 1 (range 40))
          result (promise)
          ;; a daemon thread, so a walk that does not finish fails this test and does not hold the JVM open
          _      (doto (Thread. ^Runnable #(deliver result (some? (pt/parse Other {:user-id 1, :card-id 1, :title shared}))))
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
      (is (true? (on-small-stack #(some? (pt/parse Other {:user-id 1, :card-id 1, :title (nest 990)})))))
      (is (= ::pt/bad-input (on-small-stack #(:type (refusal (nest 1200)))))))))

(deftest shared-structure-is-walked-again-when-reached-deeper-test
  (testing "a collection already walked near the top is walked again when it is reached deeper, so the limit holds"
    (let [inner (reduce (fn [x _] [x]) :x (range 900))
          outer (reduce (fn [x _] [x]) inner (range 300))]
      (is (= ::pt/bad-input (:type (refusal [inner outer]))))
      (is (= ::pt/bad-input (:type (refusal [outer inner]))))
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title [inner inner]})))))
  (testing "and a mutable value it holds is found on either path"
    (let [inner [(atom 1)]]
      (is (= [1 0] (:path (refusal [[] inner [inner]])))))))

(deftest a-clock-is-refused-test
  (testing "a clock reads the time each time it is asked, though its class is a final java.time class"
    (doseq [clock [(java.time.Clock/systemUTC) (java.time.Clock/fixed java.time.Instant/EPOCH java.time.ZoneOffset/UTC)
                   (java.time.Clock/offset (java.time.Clock/systemUTC) (java.time.Duration/ofSeconds 1))]]
      (is (= ::pt/bad-input (:type (refusal clock))) (str (class clock)))
      (is (re-find #"holds a clock, which reads the time each time it is asked.*Pass the instant it reads, as in \(\.instant clock\)\."
                   (try (pt/parse Other {:user-id 1, :card-id 1, :title clock}) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
  (testing "the instant it reads is fine"
    (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title (.instant (java.time.Clock/systemUTC))})))))

(deftest a-deep-path-is-truncated-in-the-message-test
  (let [nested (reduce (fn [x _] [x]) (atom 1) (range 20))
        msg    (try (pt/parse Other {:user-id 1, :card-id 1, :title nested}) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (re-find #"at \[0 0 0 0 0 0 0 0\] and 12 levels more holds an atom" msg))
    (is (= 20 (count (:path (refusal nested)))) "the ex-data keeps the whole path")))

(deftest nil-past-the-depth-limit-is-refused-test
  (testing "nil is a leaf, but one nested past the limit is too deep, not a NullPointerException"
    (let [data (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title (nth (iterate vector nil) 1001)}))]
      (is (= {:rule :parsed-types.rule/value-too-deep, :got "nil"} (select-keys data [:rule :got]))))))

(deftype HandMade [] parsed_types.core.ParsedValue (issuedWith [_ _] false))

(deftest a-hand-made-parsed-value-in-a-field-is-never-parsed-test
  (let [message (try (pt/parse Other {:user-id 1, :card-id 1, :title (->HandMade)}) nil
                     (catch clojure.lang.ExceptionInfo e (ex-message e)))]
    (is (str/includes? message "A class that only implements ParsedValue is never parsed"))
    (is (not (str/includes? message "pt/parse HandMade")) "a class with no plan has no input to suggest")))

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
      (is (true? (on-daemon 1000 #(some? (pt/parse Other {:user-id 1, :card-id 1, :title chain})))))))
  (testing "and one whose deepest value is past the limit from where it is reached again is refused there"
    (let [inner (reduce (fn [x _] [x]) :x (range 600))
          chain (reduce (fn [acc _] [inner acc]) [] (range 500))]
      (is (= :parsed-types.rule/value-too-deep (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title chain}))))))))

(deftest a-shared-collection-of-leaves-counts-its-leaves-test
  (testing "a vector holding only leaves is one level high, so reached again at the limit it is walked again"
    (let [inner [1]
          nest  (fn [n] (reduce (fn [x _] [x]) inner (range n)))]
      ;; inner's 1 sits at depth 1001 in the second path, and at 1000 in the third
      (is (= :parsed-types.rule/value-too-deep (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title [inner (nest 999)]})))))
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title [inner (nest 998)]})))))
  (testing "and a vector holding one is two levels high"
    (let [inner [[1]]
          nest  (fn [n] (reduce (fn [x _] [x]) inner (range n)))]
      (is (= :parsed-types.rule/value-too-deep (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title [inner (nest 998)]})))))
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title [inner (nest 997)]}))))))

(deftest a-shared-collection-of-leaves-is-walked-once-test
  (testing "it is recorded like any other collection, so a chain that reaches it at every level walks it once"
    (let [shared (vec (range 10))
          chain  (reduce (fn [acc _] [shared acc]) [] (range 50))
          walks  (atom 0)
          f      @#'pt/children-or-refusal]
      (with-redefs [pt/children-or-refusal (fn [v path acc] (when (identical? v shared) (swap! walks inc)) (f v path acc))]
        (pt/parse Other {:user-id 1, :card-id 1, :title chain}))
      (is (= 1 @walks)))))

(deftest a-value-too-deep-test
  (let [nested (reduce (fn [x _] [x]) 1 (range 1001))
        data   (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title nested}))]
    (testing "is its own rule, with its path cut to the first steps, and its depth"
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/value-too-deep, :field :title, :path [0 0 0 0 0 0 0 0]
              :depth 1001}
             (select-keys data [:type :rule :field :path :depth]))))
    (testing "and each fix the message names parses"
      (is (= [1] (:title (pt/parsed Other (pt/parse Other {:user-id 1, :card-id 1, :title (vec (flatten nested))})))))
      (is (string? (:title (pt/parsed Other (pt/parse Other {:user-id 1, :card-id 1, :title (pr-str nested)}))))))))

(deftest java-sql-dates-test
  (testing "java.sql.Date and java.sql.Time throw on .toInstant, so each message names a conversion that works"
    (doseq [[v fix convert] [[(java.sql.Date. 0) "(.toLocalDate d)" #(.toLocalDate ^java.sql.Date %)]
                             [(java.sql.Time. 0) "(.toLocalTime d)" #(.toLocalTime ^java.sql.Time %)]
                             [(java.sql.Timestamp. 0) "(.toInstant d)" #(.toInstant ^java.sql.Timestamp %)]]]
      (let [message (try (pt/parse Other {:user-id 1, :card-id 1, :title v}) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))]
        (is (str/includes? (str message) fix) (str (class v)))
        (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title (convert v)})) (str (class v)))))))

(deftest an-issued-value-in-a-field-is-not-walked-again-test
  (let [inner  (pt/parse Other {:user-id 1, :card-id 1, :title [[1] [2] [3]]})
        walked (atom 0)
        f      @#'pt/children-or-refusal]
    (with-redefs [pt/children-or-refusal (fn [v path acc] (swap! walked inc) (f v path acc))]
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title inner}))))
    (is (= 1 @walked) "the issued value itself, not the vectors inside it")))

;;; A check body that throws

(defn- boom! [^String message] (throw (IllegalStateException. message)))

(def ^:private remote-rows (fn [tuples] (into {} (map (fn [t] [t :row])) tuples)))

(pt/defparsed Throwing [amount note]
  (pure ::amount-positive "The amount is positive." [amount] (log! ::amount-positive) (pos? amount))
  (pure ::amount-small "The amount is below 100." [amount] (log! ::amount-small) (< amount 100))
  (pure ::note-string "The note is a string." [note] (log! ::note-string) (string? note))
  (external ::note-known "The note is known." [note] (log! ::note-known) (string? note)))

(pt/defparsed Remote [id]
  (external ::first-throws "Throws for id 1." [id] [row ::row] (log! ::first-throws) (if (= 1 id) (boom! "first") (some? row)))
  (external ::second-throws "Throws for ids 1 and 2." [id] (log! ::second-throws) (if (#{1 2} id) (boom! "second") true))
  (external ::id-is-3 "The id is 3." [id] (log! ::id-is-3) (= 3 id))
  (fetch ::row [id] remote-rows))

(defn- thrown [thunk]
  (try (thunk) nil (catch Throwable t t)))

(deftest a-check-that-throws-stops-the-parse-test
  (testing "a pure check that throws is that check's failure, and its exception is the cause"
    (let [e    (thrown #(pt/parse Throwing {:amount :x, :note "n"}))
          data (ex-data e)]
      (is (instance? ClassCastException (:threw data)))
      (is (= {:check ::amount-positive, :doc "The amount is positive.", :kind :pure} (select-keys data [:check :doc :kind])))
      (is (not (contains? data :returned)))
      (is (identical? (:threw data) (ex-cause e)) "the exception is the cause")
      (is (= [::amount-positive] (runs #(thrown (fn [] (pt/parse Throwing {:amount :x, :note "n"})))))
          "no check after it runs")))
  (testing "an external check that throws stops the parse too"
    (let [e    (thrown #(pt/parse Remote {:id 1}))
          data (ex-data e)]
      (is (= {:type ::pt/invalid, :check ::first-throws, :kind :external} (select-keys data [:type :check :kind])))
      (is (= "first" (ex-message (ex-cause e))))
      (is (= [::first-throws] (remove vector? (runs #(thrown (fn [] (pt/parse Remote {:id 1})))))))))
  (testing "no check threw: there is :returned and no :threw"
    (let [data (ex-data (thrown #(pt/parse Remote {:id 4})))]
      (is (= {:check ::id-is-3, :returned false} (select-keys data [:check :returned :threw])))))
  (testing "the message names what the check threw"
    (is (str/includes? (ex-message (thrown #(pt/parse Remote {:id 1})))
                       ":parsed-types.core-test/first-throws (Throws for id 1.) threw IllegalStateException: first")))
  (testing "in parse-all, a thrown pure check of a later map comes before any external check"
    (let [ran-checks (runs #(thrown (fn [] (pt/parse-all Throwing [{:amount 1, :note "n"} {:amount :x, :note "n"}]))))
          data       (ex-data (thrown #(pt/parse-all Throwing [{:amount 1, :note "n"} {:amount :x, :note "n"}])))]
      (is (= {:check ::amount-positive, :index 1} (select-keys data [:check :index])))
      (is (not-any? #{::note-known} ran-checks) "the first map's external check never ran"))))

(defn- failing-fetch [_tuples] (throw (IllegalStateException. "database down")))

(pt/defparsed Fetching [id]
  (external ::found "The row is found." [] [row ::row] (some? row))
  (fetch ::row [id] failing-fetch))

(pt/defparsed Interrupted [x]
  (pure ::stops "Throws InterruptedException." [x] (throw (InterruptedException. (str x)))))

(pt/defparsed Erroring [x]
  (pure ::errs "Throws an Error." [x] (throw (AssertionError. (str x)))))

(deftest what-a-check-throws-that-is-not-recorded-test
  (testing "an exception out of a fetch propagates unchanged"
    (let [e (thrown #(pt/parse Fetching {:id 1}))]
      (is (instance? IllegalStateException e))
      (is (= "database down" (ex-message e)))))
  (testing "an Error propagates"
    (is (instance? AssertionError (thrown #(pt/parse Erroring {:x 1})))))
  (testing "an InterruptedException propagates, with the thread's interrupt flag set again"
    (Thread/interrupted)
    ;; read the flag before any assertion: clojure.test counts in a transaction, which can clear it
    (let [e     (thrown #(pt/parse Interrupted {:x 1}))
          flag? (Thread/interrupted)]
      (is (instance? InterruptedException e))
      (is (true? flag?) "the flag is set again; Thread/interrupted cleared it"))))

;;; Immutable JDK values

(defn- refusal-message [v]
  (try (pt/parse Other {:user-id 1, :card-id 1, :title v}) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(deftest immutable-jdk-values-are-accepted-test
  (testing "URI, Locale, Currency, a Path of the default file system, enum constants and an Optional of one pass"
    (doseq [v [(java.net.URI. "https://example.com/a?b=1") java.util.Locale/US (java.util.Currency/getInstance "EUR")
               (java.nio.file.Path/of "tmp" (into-array String ["a.txt"])) java.util.concurrent.TimeUnit/SECONDS
               java.math.RoundingMode/HALF_UP (java.util.Optional/empty) (java.util.Optional/of 1)
               (java.util.Optional/of [1 {:a (java.util.Optional/of "x")}]) {:at (java.net.URI. "urn:a")}]]
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title v})) (pr-str v))))
  (testing "an Optional's value is walked"
    (is (= {:type ::pt/bad-input, :field :title, :path '[<value>], :got "clojure.lang.Atom"}
           (refusal (java.util.Optional/of (atom 1)))))
    (is (= '[:k <value> 1] (:path (refusal {:k (java.util.Optional/of [1 (int-array 1)])})))))
  (testing "a Path of another file system, or a class that only implements Path, is not known to be immutable"
    (let [jrt (java.nio.file.FileSystems/getFileSystem (java.net.URI. "jrt:/"))]
      (doseq [v [(.getPath jrt "modules" (make-array String 0)) (reify java.nio.file.Path)]]
        (is (= :parsed-types.rule/not-known-immutable
               (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title v}))))
            (.getName (class v))))))
  (testing "a URL is refused, since its equals looks the host up, and the fix the message names parses"
    (let [url (java.net.URL. "https://example.com/a")]
      (is (= :parsed-types.rule/url-value (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title url})))))
      (is (str/includes? (refusal-message url) "Use a java.net.URI, as in (.toURI url)."))
      (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title (.toURI url)})))))
  (testing "any other class is refused as not known to be immutable, never as mutable, and (str x) parses"
    (doseq [v [(Object.) (java.util.concurrent.atomic.AtomicLong. 1) (transient [])]]
      (let [message (refusal-message v)]
        (is (= :parsed-types.rule/not-known-immutable
               (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title v})))))
        (is (str/includes? message "a value of a class not known to be immutable") message)
        (is (not (re-find #"\bmutable" message)) message)
        (is (str/includes? message "as in (str x).") message)
        (is (some? (pt/parse Other {:user-id 1, :card-id 1, :title (str v)}))))))
  (testing "a value known to be mutable keeps its own message"
    (doseq [[v what] [[(atom 1) "holds an atom"] [(java.util.Date. 0) "holds a mutable date"]
                      [(java.util.ArrayList.) "holds a mutable Java collection"] [(map inc [1]) "holds a lazy seq"]]]
      (is (str/includes? (refusal-message v) what))
      (is (= :parsed-types.rule/mutable-value (:rule (thrown-data #(pt/parse Other {:user-id 1, :card-id 1, :title v}))))))))
