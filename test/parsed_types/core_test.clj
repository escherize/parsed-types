(ns parsed-types.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [clojure.walk :as walk]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;; What ran, in order. A check may not close over a local, so the log is a var.
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
            :failed-kinds #{:external}
            :draft        "parsed_types.core_test.RenameCardDraft"
            :failures     {::may-write-card {:doc "User 1 may write the card.", :returned false}}}
           (thrown-data #(pt/parse (->RenameCardDraft 1 3 "New"))))))
  (testing "who is acting is a field: the same card, another user"
    (is (= #{:external} (:failed-kinds (thrown-data #(pt/parse (->RenameCardDraft 2 1 "New"))))))))

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
               (thrown-data #(pt/parsed RenameCard x))))
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
    (is (= {:type ::pt/bad-input, :got "parsed_types.core_test.LyingDraft"}
           (thrown-data #(pt/parse (->LyingDraft 5)))))))

(pt/defparsed Closure [a]
  (external ::known "a is in the db." [] [row ::row] (some? row))
  (fetch ::row [a] (let [db {1 :one}] (fn [tuples] (into {} (map (fn [[id :as t]] [t (get db id)])) tuples)))))

(deftest a-fetch-may-close-over-a-local-test
  (testing "a fetch reads the world, so it may hold a database handle; only pure checks must close over nothing"
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
    (doseq [[v message] [[{:a [1 (atom 1)]} "OtherDraft: the field :title at [:a 1] holds an atom; deref it."]
                         [(java.util.Date. 0) "OtherDraft: the field :title holds java.util.Date is mutable; use java.time.Instant."]
                         [(map inc [1]) "OtherDraft: the field :title holds a lazy seq; pass (vec ...)."]
                         [(int-array 1) "OtherDraft: the field :title holds a Java array; copy it into a vector."]]]
      (is (= message (try (pt/parse (->OtherDraft 1 1 v)) nil (catch clojure.lang.ExceptionInfo e (ex-message e)))))))
  (testing "immutable values pass: scalars, java.time values, persistent collections, records and parsed values"
    (doseq [v [nil true \a "s" :k 'sym 1 (int 1) 1.5 1M 1N 1/3 (biginteger 1) (random-uuid) #"re" String
               (java.time.Instant/now) (java.time.LocalDate/of 2026 1 1) (java.time.ZoneId/of "Europe/Paris")
               (java.time.Duration/ofSeconds 1) java.time.DayOfWeek/MONDAY
               [1 [2 #{3}] '(4 5) {:a {:b (java.time.Instant/now)}}] (sorted-map :a 1) clojure.lang.PersistentQueue/EMPTY
               (->Point 1 [2]) (pt/parse (->OtherDraft 1 1 nil))]]
      (is (some? (pt/parse (->OtherDraft 1 1 v))) (pr-str v)))))

(deftest error-messages-name-the-fix-test
  (let [real    (pt/parse (->RenameCardDraft 1 1 "New"))
        message (fn [thunk] (try (thunk) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))]
    (is (= "RenameCard is a parsed value and cannot be changed with assoc. Use (pt/draft-of x), assoc, then pt/parse."
           (message #(assoc real :title "x"))))
    (is (= (str "Expected a parsed RenameCard, got a RenameCard that parse did not issue. Build a RenameCardDraft and"
                " call pt/parse.")
           (message #(pt/parsed RenameCard (RenameCard. 1 1 "New" nil)))))
    (is (= "Expected a parsed RenameCard, got a RenameCardDraft. Call pt/parse on it first."
           (message #(pt/parsed RenameCard (->RenameCardDraft 1 1 "New")))))
    (is (= "RenameCard has fixed fields, so it has no empty version. Use (into {} x) for a plain map."
           (message #(empty real))))
    (is (= "RenameCard has no field :nope. Use one of its fields [:user-id :card-id :title], or give get a default."
           (message #(:nope real))))
    (is (str/starts-with? (message #(pt/parse (->RenameCardDraft 1 -1 " ")))
                          (str "Invalid RenameCard: :parsed-types.core-test/card-id-positive,"
                               " :parsed-types.core-test/title-not-blank failed. Change the draft so these checks"
                               " pass")))))
