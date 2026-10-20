(ns parsed-types.core-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
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
        (is (= parsed (RenameCard. 1 1 "New" nil)) "it equals a value of its type with the same fields, parsed or not")
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
           (runs #(pt/check (->RenameCardDraft 1 -1 "New") nil))))
    (is (= [::card-id-positive ::title-not-blank]
           (runs #(pt/check (->RenameCardDraft 1 1 " ") nil))))))

(deftest check-test
  (let [draft (->RenameCardDraft 1 1 "New")]
    (testing "check runs what it names and what that waits on"
      (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id]
             (runs #(is (true? (pt/check draft [::title-not-the-card-id])))))))
    (testing "a check issues nothing: parse runs every check again"
      (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
             (runs #(pt/parse draft)))))
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
    (is (= ::pt/bad-input (:type (thrown-data #(pt/check (reify parsed_types.core.DraftValue) nil)))))))

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
    (testing "equality, hash and print ignore it, so a forgery is equal: proof is pt/parsed, never ="
      (is (= real (RenameCard. 1 1 "New" nil)))
      (is (= (hash real) (hash (RenameCard. 1 1 "New" nil))))
      (is (= (pr-str real) (pr-str (RenameCard. 1 1 "New" nil))))
      (is (not (re-find #"proof" (pr-str real)))))))

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
    (is (= "RenameCard has no field :nope. Use one of its fields [:user-id :card-id :title], or give get a default."
           (message #(:nope real))))
    (is (str/starts-with? (message #(pt/parse (->RenameCardDraft 1 -1 " ")))
                          (str "Invalid RenameCard: :parsed-types.core-test/card-id-positive,"
                               " :parsed-types.core-test/title-not-blank failed. Change the draft so these checks"
                               " pass")))))
