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

(pt/defparsed-type RenameCard [user-id card-id title])

(def ^:private rename-card-declaration
  {:parses-to RenameCard
   :checks    [{:name  ::card-id-positive
                :doc   "The card id is a positive integer."
                :kind  :pure
                :reads [:card-id]
                :f     (fn [card-id] (log! ::card-id-positive) (pos-int? card-id))}
               {:name  ::title-not-blank
                :doc   "The title is a non-blank string."
                :kind  :pure
                :reads [:title]
                :f     (fn [title]
                         (log! ::title-not-blank)
                         (or (and (string? title) (not (str/blank? title)))
                             {:title title}))}
               {:name  ::title-not-the-card-id
                :doc   "The title is not the card id."
                :kind  :pure
                :reads [:card-id :title]
                :f     (fn [card-id title] (log! ::title-not-the-card-id) (not= (str card-id) title))}
               {:name    ::may-write-card
                :doc     "User 1 may write the card."
                :kind    :external
                :reads   [:user-id]
                :fetches [::card]
                :f       (fn [user-id card] (log! ::may-write-card) (and (= 1 user-id) (:writable? card)))}]
   :fetches   [{:name  ::card
                :reads [:card-id]
                :f     (fn [arg-tuples]
                         (log! [::card arg-tuples])
                         (into {} (map (fn [[id :as t]] [t {:id id, :writable? (contains? writable-card-ids id)}]))
                               arg-tuples))}]})

(pt/defdraft-type RenameCardDraft [user-id card-id title]
  rename-card-declaration)

(pt/defparsed-type Other [user-id card-id title])

(def ^:private other-declaration {:parses-to Other, :checks []})

(pt/defdraft-type OtherDraft [user-id card-id title]
  other-declaration)

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
        (is (= parsed (->RenameCard 1 1 "New")) "it equals a value of its type with the same fields, parsed or not")
        (is (not= parsed (->RenameCardDraft 1 1 "New")) "and not its draft")
        (is (= 1 (:card-id parsed)))
        (is (= 1 (.-card-id ^RenameCard parsed)))
        (is (= 500 (:status-code (thrown-data #(:nope parsed)))) "a key that is not a field throws")
        (is (= ::none (get parsed :nope ::none)) "unless a default is given")
        (is (not (contains? parsed :nope))))
      (testing "it prints with its type"
        (is (= "#parsed_types.core_test.RenameCard{:user-id 1, :card-id 1, :title \"New\"}" (pr-str parsed))))))
  (testing "a failed pure check is a 400 reporting every failure by name, with its doc and what it returned"
    (is (= {:status-code 400
            :draft       RenameCardDraft
            :failures    {::card-id-positive {:doc      "The card id is a positive integer."
                                              :returned false
                                              :blocked  [::title-not-the-card-id ::may-write-card]}
                          ::title-not-blank  {:doc      "The title is a non-blank string."
                                              :returned {:title " "}
                                              :blocked  [::title-not-the-card-id ::may-write-card]}}}
           (thrown-data #(pt/parse (->RenameCardDraft 1 -1 " "))))))
  (testing "a failed external check is a 403"
    (is (= {:status-code 403
            :draft       RenameCardDraft
            :failures    {::may-write-card {:doc "User 1 may write the card.", :returned false}}}
           (thrown-data #(pt/parse (->RenameCardDraft 1 3 "New"))))))
  (testing "who is acting is a field: the same card, another user"
    (is (= 403 (:status-code (thrown-data #(pt/parse (->RenameCardDraft 2 1 "New"))))))))

(deftest order-test
  (testing "shape checks first, then checks over several fields, then fetches, then external checks"
    (is (= [::card-id-positive ::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
           (runs #(pt/parse (->RenameCardDraft 1 1 "New"))))))
  (testing "a failed pure check holds up every fetch and external check, even one that does not read its field"
    (is (= [::card-id-positive ::title-not-blank]
           (runs #(pt/check (->RenameCardDraft 1 -1 "New") nil))))
    (is (= [::card-id-positive ::title-not-blank]
           (runs #(pt/check (->RenameCardDraft 1 1 " ") nil))))))

(deftest remembered-passes-test
  (let [draft (->RenameCardDraft 1 1 "New")]
    (testing "check runs what it names and what that waits on, and remembers the pure checks that passed"
      (is (true? (pt/check draft [::title-not-the-card-id])))
      (is (= #{::card-id-positive ::title-not-blank ::title-not-the-card-id}
             (pt/passed-checks draft))))
    (testing "parse runs only what is not known to pass; external checks run every time"
      (is (= [[::card [[1]]] ::may-write-card]
             (runs #(pt/parse draft)))))
    (testing "assoc keeps the passes of the checks that do not read the field it writes"
      (let [renamed (assoc draft :title "Newer")]
        (is (= #{::card-id-positive} (pt/passed-checks renamed)))
        (is (= [::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
               (runs #(pt/parse renamed))))))
    (testing "assoc refuses a key that is not a field, and dissoc any key"
      (is (= 500 (:status-code (thrown-data #(assoc draft :titel "x")))))
      (is (= 500 (:status-code (thrown-data #(dissoc draft :title))))))
    (testing "a failure is not remembered"
      (let [bad (->RenameCardDraft 1 1 " ")]
        (pt/check bad nil)
        (is (not (contains? (pt/passed-checks bad) ::title-not-blank)))
        (is (= [::title-not-blank] (runs #(pt/check bad [::title-not-blank]))))))))

(deftest draft-of-test
  (let [parsed (pt/parse (->RenameCardDraft 1 1 "New"))
        draft  (pt/draft-of parsed)]
    (testing "a parsed value goes back to a draft that is known to pass every pure check"
      (is (= (->RenameCardDraft 1 1 "New") draft))
      (is (= #{::card-id-positive ::title-not-blank ::title-not-the-card-id} (pt/passed-checks draft))))
    (testing "editing it reruns only the checks that read the field, and the external checks"
      (is (= [::title-not-blank ::title-not-the-card-id [::card [[1]]] ::may-write-card]
             (runs #(pt/parse (assoc draft :title "Newer"))))))
    (testing "draft-of refuses anything parse did not issue"
      (is (= 500 (:status-code (thrown-data #(pt/draft-of (->RenameCard 1 1 "New")))))))))

(pt/defparsed-type Moded [user-id card-id title])

(def ^:private lax-declaration
  {:parses-to Moded
   :checks    [{:name ::lax, :doc "Any title.", :kind :pure, :reads [:title], :f (fn [_] true)}]})

(def ^:private strict-declaration
  {:parses-to Moded
   :checks    [{:name ::strict, :doc "A positive card id.", :kind :pure, :reads [:card-id], :f pos-int?}]})

(pt/defdraft-type ModedDraft [user-id card-id title]
  (if (= :strict user-id) strict-declaration lax-declaration))

(deftest remembered-passes-belong-to-a-plan-test
  (testing "passes remembered under one declaration are not read under another"
    (let [lax (->ModedDraft :lax -1 "t")]
      (pt/check lax nil)
      (is (= #{::lax} (pt/passed-checks lax)))
      (let [strict (assoc lax :user-id :strict)]
        (is (= #{} (pt/passed-checks strict)))
        (is (= 400 (:status-code (thrown-data #(pt/parse strict)))))))))

(deftest batching-test
  (testing "parse-all calls each fetch once, with each distinct tuple once"
    (let [drafts [(->RenameCardDraft 1 1 "a") (->RenameCardDraft 1 2 "b") (->RenameCardDraft 1 1 "c")]
          parsed (atom nil)]
      (is (= [[::card [[1] [2]]]]
             (filter vector? (runs #(reset! parsed (pt/parse-all drafts))))))
      (is (= 3 (count @parsed)))
      (is (every? #(pt/parsed? RenameCard %) @parsed))))
  (testing "parse-all throws once with every failure, by index"
    (is (= {:status-code 403
            :failures    {1 {::may-write-card {:doc "User 1 may write the card.", :returned false}}
                          2 {::card-id-positive {:doc      "The card id is a positive integer."
                                                 :returned false
                                                 :blocked  [::title-not-the-card-id ::may-write-card]}}}}
           (thrown-data #(pt/parse-all [(->RenameCardDraft 1 1 "a")
                                        (->RenameCardDraft 1 3 "b")
                                        (->RenameCardDraft 1 -1 "c")]))))))

(pt/defparsed-type NotFound [])

(def ^:private not-found-declaration
  {:parses-to NotFound
   :checks    [{:name        ::exists
                :doc         "The row exists."
                :kind        :external
                :status-code 404
                :f           (fn [] false)}]})

(pt/defdraft-type NotFoundDraft []
  not-found-declaration)

(deftest status-code-test
  (is (= 404 (:status-code (thrown-data #(pt/parse (->NotFoundDraft)))))))

;; The declaration under test. Each one is a new map, so each is planned afresh.
(def ^:private bad-declaration (atom nil))

(pt/defparsed-type BadParsed [a])
(pt/defparsed-type WrongFields [b])

(pt/defdraft-type BadDraft [a]
  @bad-declaration)

(deftest declaration-errors-test
  (let [x 1]
    (doseq [[label decl]
            [["no :parses-to"
              {:checks []}]
             ["a :parses-to that is not a parsed type"
              {:parses-to String, :checks []}]
             ["a :parses-to that is a record, which assoc would change"
              {:parses-to BadDraft, :checks []}]
             ["a parsed type with other fields"
              {:parses-to WrongFields, :checks []}]
             ["a check that closes over a local"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a], :f (fn [a] (= a x))}]}]
             ["a check built with constantly"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a], :f (constantly true)}]}]
             ["a check built with partial"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a], :f (partial = 1)}]}]
             ["a check built with comp"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a], :f (comp boolean pos-int?)}]}]
             ["a check whose :f is a keyword, not a fn"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a], :f :a}]}]
             ["a check with no :f"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a]}]}]
             ["an unqualified name"
              {:parses-to BadParsed, :checks [{:name :c, :doc "d", :kind :pure, :reads [:a], :f (fn [_] true)}]}]
             [":reads that is not a vector"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads :a, :f (fn [_] true)}]}]
             [":reads that names a field twice"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a :a], :f (fn [_ _] true)}]}]
             ["a type another draft parses to"
              {:parses-to RenameCard, :checks []}]
             ["a child field the draft does not have"
              {:parses-to BadParsed, :checks [], :children {:b {:kind :many, :type BadParsed}}}]
             ["a child that is not {:kind :type}"
              {:parses-to BadParsed, :checks [], :children {:a {:kind :lots, :type BadParsed}}}]
             ["more than 64 pure checks"
              {:parses-to BadParsed
               :checks    (vec (for [i (range 65)]
                                 {:name (keyword "parsed-types.core-test" (str "c" i)), :doc "d", :kind :pure,
                                  :reads [], :f (fn [] true)}))}]
             ["a check built with memoize"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:a], :f (memoize pos-int?)}]}]
             ["a field the draft does not have"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :pure, :reads [:b], :f (fn [_] true)}]}]
             ["no :doc"
              {:parses-to BadParsed, :checks [{:name ::c, :kind :pure, :reads [:a], :f (fn [_] true)}]}]
             ["no :kind"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :reads [:a], :f (fn [_] true)}]}]
             ["a pure check that fetches"
              {:parses-to BadParsed
               :checks    [{:name ::c, :doc "d", :kind :pure, :fetches [::f], :f (fn [_] true)}]
               :fetches   [{:name ::f, :reads [], :f (fn [_] {})}]}]
             ["an undeclared fetch"
              {:parses-to BadParsed, :checks [{:name ::c, :doc "d", :kind :external, :fetches [::f], :f (fn [_] true)}]}]
             ["a name used twice"
              {:parses-to BadParsed
               :checks    [{:name ::c, :doc "d", :kind :pure, :f (fn [] true)}
                           {:name ::c, :doc "d", :kind :pure, :f (fn [] true)}]}]]]
      (testing label
        (reset! bad-declaration decl)
        (is (= 500 (:status-code (thrown-data #(pt/parse (->BadDraft 1))))))))))

(pt/defdraft-type BadDraft2 [a]
  @bad-declaration)

(deftest shared-declaration-test
  (testing "two draft types may not share one declaration: the second would parse to a type the first owns"
    (reset! bad-declaration {:parses-to BadParsed, :checks []})
    (pt/parse (->BadDraft 1))
    (is (re-find #"BadParsed is parsed from another draft type"
                 (ex-message (try (pt/parse (->BadDraft2 1)) (catch clojure.lang.ExceptionInfo e e)))))))

(deftest a-draft-must-be-a-draft-type-test
  (testing "anything but a type defparsed or defdraft-type made is refused"
    (is (= 500 (:status-code (thrown-data #(pt/parse (reify pt/Draft (declaration [_] rename-card-declaration)))))))))

(deftest parsed-refuses-anything-parse-did-not-issue-test
  (let [real (pt/parse (->RenameCardDraft 1 1 "New"))]
    (doseq [[label x] [["nil" nil]
                       ["a plain map" {:user-id 1, :card-id 3, :title "pwned"}]
                       ["the draft" (->RenameCardDraft 1 1 "New")]
                       ["a parsed type built with its constructor" (->RenameCard 1 3 "pwned")]
                       ["a value equal to a parsed one, built by hand" (->RenameCard 1 1 "New")]
                       ["a parsed value of another type" (pt/parse (->OtherDraft 1 3 "pwned"))]
                       ["an edn round trip" (edn/read-string {:default (fn [_ m] m)} (pr-str real))]]]
      (testing label
        (is (= {:status-code 500, :expected RenameCard, :got (type x)}
               (thrown-data #(pt/parsed RenameCard x))))
        (is (false? (pt/parsed? RenameCard x)))))
    (is (true? (pt/parsed? RenameCard real)))))

(deftest a-parsed-value-cannot-be-changed-test
  (let [real (pt/parse (->RenameCardDraft 1 1 "New"))]
    (doseq [[label thunk] [["assoc" #(assoc real :card-id 3)]
                           ["dissoc" #(dissoc real :card-id)]
                           ["merge" #(merge real {:card-id 3})]
                           ["update" #(update real :card-id inc)]
                           ["with-meta" #(with-meta real {:x 1})]
                           ["vary-meta" #(vary-meta real assoc :x 1)]]]
      (testing label
        (is (= 500 (:status-code (thrown-data thunk))))))
    (testing "the draft type is a record, and the parse is still the only way to the parsed one"
      (is (= 500 (:status-code (thrown-data #(assoc real :card-id 3))))))))

(deftest extend-cannot-replace-the-checks-test
  (testing "Clojure refuses to extend a draft to Draft when it implements it inline, so its checks cannot be emptied"
    (is (thrown-with-msg? IllegalArgumentException #"already directly implements"
                          (extend-type RenameCardDraft
                            pt/Draft
                            (declaration [_] {:parses-to RenameCard, :checks []}))))))

(pt/defparsed-type Claimed [a])

(def ^:private claim-a-declaration {:parses-to Claimed, :checks []})
(def ^:private claim-b-declaration {:parses-to Claimed, :checks []})

(pt/defdraft-type ClaimADraft [a]
  claim-a-declaration)

(pt/defdraft-type ClaimBDraft [a]
  claim-b-declaration)

(deftest one-draft-type-per-parsed-type-test
  (testing "a second draft type with the same fields may not parse to a type another already parses to"
    (is (pt/parsed? Claimed (pt/parse (->ClaimADraft 1))))
    (is (re-find #"Claimed is parsed from another draft type"
                 (ex-message (try (pt/parse (->ClaimBDraft 1)) (catch clojure.lang.ExceptionInfo e e)))))))

(pt/defparsed-type Honest [a])

(def ^:private lying-declaration
  {:parses-to Honest
   :checks    [{:name ::a-positive, :doc "a is positive.", :kind :pure, :reads [:a], :f (fn [a] (pos-int? a))}]})

;; Claims to be a draft, and answers `get` with a value its field does not hold.
(deftype LyingDraft [a]
  parsed_types.core.DraftValue
  clojure.lang.ILookup
  (valAt [_ k] (when (= :a k) -999))
  (valAt [_ k not-found] (if (= :a k) -999 not-found))
  pt/Draft
  (declaration [_] lying-declaration))

(deftest fields-are-read-once-from-the-record-test
  (testing "the checks and the issued value both see the field, not what get answers"
    (let [parsed (pt/parse (->LyingDraft 5))]
      (is (= {:a 5} (into {} parsed))))))

(pt/defparsed-type Closure [a])

(def ^:private fetch-closure-declaration
  {:parses-to Closure
   :checks    [{:name ::known, :doc "a is in the db.", :kind :external, :fetches [::row]
                :f    (fn [row] (some? row))}]
   :fetches   [{:name  ::row
                :reads [:a]
                :f     (let [db {1 :one}] (fn [tuples] (into {} (map (fn [[a :as t]] [t (get db a)])) tuples)))}]})

(pt/defdraft-type ClosureDraft [a]
  fetch-closure-declaration)

(deftest a-fetch-may-close-over-a-local-test
  (testing "a fetch reads the world, so it may hold a database handle; only pure checks must close over nothing"
    (is (= {:a 1} (into {} (pt/parse (->ClosureDraft 1)))))
    (is (= 403 (:status-code (thrown-data #(pt/parse (->ClosureDraft 2))))))))

(deftest a-hand-written-declaration-may-not-use-reserved-names-test
  (reset! bad-declaration {:parses-to BadParsed
                           :checks    [{:name :parsed-types.child/a, :doc "d", :kind :pure, :reads [:a], :f (fn [_] true)}]})
  (is (re-find #"reserved for child-field checks"
               (ex-message (try (pt/parse (->BadDraft 1)) (catch clojure.lang.ExceptionInfo e e))))))
