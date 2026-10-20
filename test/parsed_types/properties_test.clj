(ns parsed-types.properties-test
  "Generative tests of the properties the library promises, over random drafts and random sequences of edits."
  (:require
   [clojure.set :as set]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;; What ran, in order. A check may not close over a local, so the log is a var.
(def ^:private ran (atom []))

(defn- log! [x] (swap! ran conj x) true)

(defn- rows [arg-tuples]
  (log! [::row arg-tuples])
  (into {} (map (fn [[a :as t]] [t {:ok? (even? a)}])) arg-tuples))

(pt/defparsed Triple [user-id a b c]
  (pure ::a-int "a is an integer." [a] (log! ::a-int) (int? a))
  (pure ::b-string "b is a string." [b] (log! ::b-string) (string? b))
  (pure ::c-int "c is an integer." [c] (log! ::c-int) (int? c))
  ;; same fields as ::b-string, so it runs whatever that returns, and must accept any b
  (pure ::b-short "b has at most 3 characters." [b] (log! ::b-short) (or (<= (count (str b)) 3) {:length (count (str b))}))
  (pure ::a-below-c "a is below c." [a c] (log! ::a-below-c) (< a c))
  (external ::may-use-row "User 1 may use the row for a."
            [user-id] [row ::row]
            (log! ::may-use-row)
            (and (= 1 user-id) (:ok? row)))
  (external ::user-known "The user is known. An external check with no fetch."
            [user-id]
            (log! ::user-known)
            (contains? #{1 2} user-id))
  (fetch ::row [a] rows))

(def ^:private fields [:user-id :a :b :c])

(def ^:private pure-names #{::a-int ::b-string ::c-int ::b-short ::a-below-c})

(def ^:private touches-world? #{::may-use-row ::user-known ::row})

(def ^:private gen-value
  (gen/one-of [gen/small-integer gen/small-integer gen/string-alphanumeric (gen/return nil)]))

(def ^:private gen-any-draft
  (gen/fmap (fn [[u a b c]] (->TripleDraft u a b c))
            (gen/tuple (gen/elements [1 2 3]) gen-value gen-value gen-value)))

(def ^:private gen-well-formed-draft
  "A draft that passes every pure check, and any user and parity, so the external checks fail about half the time."
  (gen/fmap (fn [[u a b gap]] (->TripleDraft u a (subs b 0 (min 3 (count b))) (+ a gap 1)))
            (gen/tuple (gen/elements [1 1 2 3]) gen/small-integer gen/string-alphanumeric gen/nat)))

(def ^:private gen-valid-draft
  "A draft that passes every check: user 1, an even a, a short b, and c above a."
  (gen/fmap (fn [[half b gap]] (->TripleDraft 1 (* 2 half) (subs b 0 (min 3 (count b))) (+ (* 2 half) gap 1)))
            (gen/tuple gen/small-integer gen/string-alphanumeric gen/nat)))

(def ^:private gen-draft
  "Half malformed and half well-formed, so the pure, the external and the passing paths are all exercised."
  (gen/one-of [gen-any-draft gen-well-formed-draft]))

(defn- fresh
  "A new draft with the same fields as `draft`."
  [draft]
  (->TripleDraft (:user-id draft) (:a draft) (:b draft) (:c draft)))

(defn- outcome
  "What `thunk` returned, as a plain value, or the type, failed kinds and failures it threw."
  [thunk]
  (try
    (let [x (thunk)]
      {:ok (if (vector? x) (mapv #(into {} %) x) (into {} x))})
    (catch clojure.lang.ExceptionInfo e
      (select-keys (ex-data e) [:type :failed-kinds :failures]))))

(defspec nothing-touches-the-world-until-every-pure-check-passes 300
  (prop/for-all [draft gen-draft]
    (reset! ran [])
    (let [{:keys [failures]} (outcome #(pt/parse draft))
          pure-failed?       (some pure-names (keys failures))]
      (or (not pure-failed?)
          (not-any? #(touches-world? (if (vector? %) (first %) %)) @ran)))))

(defspec parse-issues-a-new-equal-value-and-nothing-else-is-parsed 300
  (prop/for-all [draft gen-draft]
    (let [forged (->Triple (:user-id draft) (:a draft) (:b draft) (:c draft))
          result (outcome #(pt/parse draft))]
      (and (not (pt/parsed? Triple forged))
           (not (pt/parsed? Triple draft))
           (not (pt/parsed? Triple (into {} draft)))
           (if (:ok result)
             (let [parsed (pt/parse (fresh draft))]
               (and (pt/parsed? Triple parsed)
                    (= (into {} parsed) (into {} draft))
                    (not= parsed (into {} draft))
                    (not= (into {} draft) parsed)
                    (not= parsed draft)
                    (= parsed forged)
                    (not (identical? parsed forged))))
             true)))))

(defspec a-parsed-value-cannot-be-changed 200
  (prop/for-all [draft gen-valid-draft
                 k     (gen/one-of [(gen/elements fields) gen/keyword])
                 v     gen-value]
    (let [parsed  (pt/parse draft)
          refused #(try (%) false
                        (catch clojure.lang.ExceptionInfo e (= ::pt/cannot-change (:type (ex-data e)))))]
      (and (refused #(assoc parsed k v))
           (refused #(dissoc parsed k))
           (refused #(merge parsed {k v}))
           (refused #(with-meta parsed {k v}))
           (pt/parsed? Triple parsed)))))

(defspec draft-of-round-trips 200
  (prop/for-all [draft gen-valid-draft]
    (let [parsed   (pt/parse draft)
          reparsed (pt/parse (pt/draft-of parsed))]
      (and (= parsed reparsed)
           (not (identical? parsed reparsed))
           (pt/parsed? Triple reparsed)))))

(defspec parse-all-agrees-with-parse 300
  (prop/for-all [drafts (gen/vector gen-draft 0 8)]
    (let [each     (mapv #(outcome (fn [] (pt/parse (fresh %)))) drafts)
          all      (outcome #(pt/parse-all (mapv fresh drafts)))
          failures (into (sorted-map) (keep-indexed (fn [i o] (when-not (:ok o) [i (:failures o)]))) each)]
      (if (empty? failures)
        (= all {:ok (mapv :ok each)})
        (= all {:type         ::pt/invalid
                :failed-kinds (apply set/union (keep :failed-kinds each))
                :failures     failures})))))

(defspec parse-all-fetches-once-with-each-distinct-tuple 300
  (prop/for-all [drafts (gen/vector gen-draft 0 8)]
    (let [pure-ok? (fn [d] (true? (pt/check (fresh d) (vec pure-names))))
          expected (into [] (comp (filter pure-ok?) (map (fn [d] [(:a d)])) (distinct)) drafts)]
      (reset! ran [])
      (outcome #(pt/parse-all (mapv fresh drafts)))
      (let [calls (filterv #(and (vector? %) (= ::row (first %))) @ran)]
        (if (empty? expected)
          (empty? calls)
          (= [[::row expected]] calls))))))

;;; A reference model: what parse must return, written out by hand from the declaration, so a bug the library makes on
;;; both sides of a comparison still fails here.

(defn- model
  "The outcome of parsing a TripleDraft with these fields, from first principles."
  [{:keys [user-id a b c], :as fields}]
  (let [externals [::may-use-row ::user-known]
        singles   [[::a-int "a is an integer." (int? a)]
                   [::b-string "b is a string." (string? b)]
                   [::c-int "c is an integer." (int? c)]
                   [::b-short "b has at most 3 characters." (or (<= (count (str b)) 3) {:length (count (str b))})]]
        failed    (into {} (for [[n doc r] singles :when (not (true? r))] [n {:doc doc, :returned r}]))
        relation  (cond
                    (or (failed ::a-int) (failed ::c-int)) :blocked
                    (< a c)                                :passed
                    :else                                  :failed)
        failed    (cond-> failed
                    (= :failed relation) (assoc ::a-below-c {:doc "a is below c.", :returned false}))
        blocked   (fn [n] (vec (concat (when (and (= :blocked relation) (#{::a-int ::c-int} n)) [::a-below-c])
                                       externals)))]
    (if (seq failed)
      {:type         ::pt/invalid
       :failed-kinds #{:pure}
       :failures     (into {} (map (fn [[n e]] [n (assoc e :blocked (blocked n))])) failed)}
      (let [ext (cond-> {}
                  (not (and (= 1 user-id) (even? a)))
                  (assoc ::may-use-row {:doc "User 1 may use the row for a.", :returned false})
                  (not (contains? #{1 2} user-id))
                  (assoc ::user-known {:doc "The user is known. An external check with no fetch.", :returned false}))]
        (if (seq ext)
          {:type ::pt/invalid, :failed-kinds #{:external}, :failures ext}
          {:ok (into {} fields)})))))

(defspec parse-matches-the-model 500
  (prop/for-all [draft gen-draft]
    (= (model draft) (outcome #(pt/parse draft)))))

(defspec the-generators-reach-every-outcome 1
  (prop/for-all [drafts (gen/vector gen-draft 400)]
    (= #{#{:pure} #{:external} :ok} (set (map #(or (:failed-kinds %) :ok) (map model drafts))))))

;;; Several declarations in one call, sharing a fetch name with different fetches

(defn- pos-rows [tuples] (into {} (map (fn [[a :as t]] [t {:ok? (and (int? a) (pos? a))}])) tuples))

(defn- neg-rows [tuples] (into {} (map (fn [[a :as t]] [t {:ok? (and (int? a) (neg? a))}])) tuples))

;; Both fetch ::row, differently.
(pt/defparsed PosPair [a]
  (external ::pair-ok "The row for a is ok." [] [row ::row] (true? (:ok? row)))
  (fetch ::row [a] pos-rows))

(pt/defparsed NegPair [a]
  (external ::pair-ok "The row for a is ok." [] [row ::row] (true? (:ok? row)))
  (fetch ::row [a] neg-rows))

(def ^:private gen-mixed-draft
  (gen/one-of [gen-draft
               (gen/fmap (fn [[positive? a]] (if positive? (->PosPairDraft a) (->NegPairDraft a)))
                         (gen/tuple gen/boolean gen/small-integer))]))

(defspec parse-all-agrees-with-parse-across-declarations 300
  (prop/for-all [drafts (gen/vector gen-mixed-draft 0 8)]
    (let [fresh-any (fn [d] (cond
                              (instance? TripleDraft d)  (fresh d)
                              (instance? PosPairDraft d) (->PosPairDraft (:a d))
                              :else                      (->NegPairDraft (:a d))))
          each      (mapv #(outcome (fn [] (pt/parse (fresh-any %)))) drafts)
          all       (outcome #(pt/parse-all (mapv fresh-any drafts)))
          failures  (into (sorted-map) (keep-indexed (fn [i o] (when-not (:ok o) [i (:failures o)]))) each)]
      (if (empty? failures)
        (= all {:ok (mapv :ok each)})
        (= all {:type         ::pt/invalid
                :failed-kinds (apply set/union (keep :failed-kinds each))
                :failures     failures})))))

;;; Trees: a node holds child nodes, and one parse covers them all

(defn- v-rows [tuples] (log! [::v-row tuples]) (into {} (map (fn [[v :as t]] [t {:v v}])) tuples))

(pt/defparsed Node [v kids]
  (children kids Node)
  (pure ::v-int "v is an integer." [v] (log! ::v-int) (int? v))
  (pure ::kids-below "Every child's v is below this one's." [v kids] (log! ::kids-below) (every? #(< (:v %) v) kids))
  (external ::v-known "v is not 13." [] [row ::v-row] (log! ::v-known) (not= 13 (:v row)))
  (fetch ::v-row [v] v-rows))

(def ^:private gen-tree
  (gen/recursive-gen (fn [inner]
                       (gen/fmap (fn [[v kids]] (->NodeDraft v kids))
                                 (gen/tuple (gen/one-of [gen/small-integer (gen/return nil)]) (gen/vector inner 0 3))))
                     (gen/fmap (fn [v] (->NodeDraft v [])) (gen/one-of [gen/small-integer gen/small-integer
                                                                         (gen/return nil)]))))

(def ^:private gen-ordered-tree
  "A tree that passes every pure check: each child's v below its parent's."
  (letfn [(tree [depth v]
            (gen/bind (gen/vector (gen/choose 1 4) 0 (if (zero? depth) 0 3))
                      (fn [gaps]
                        (gen/fmap (fn [kids] (->NodeDraft v (vec kids)))
                                  (apply gen/tuple (map #(tree (dec depth) (- v %)) gaps))))))]
    (gen/bind (gen/tuple (gen/choose 0 3) (gen/choose 0 30)) (fn [[depth v]] (tree depth v)))))

(defn- nodes-of [tree] (tree-seq #(seq (:kids %)) :kids tree))

(defn- tree-model
  "Whether the tree parses, from first principles."
  [node]
  (and (int? (:v node))
       (every? tree-model (:kids node))
       (every? #(< (:v %) (:v node)) (:kids node))
       (not-any? #(= 13 (:v %)) (nodes-of node))))

(defspec a-tree-parses-exactly-when-every-node-passes 300
  (prop/for-all [tree (gen/one-of [gen-tree gen-ordered-tree])]
    (= (tree-model tree) (contains? (outcome #(pt/parse tree)) :ok))))

(defspec nothing-in-a-tree-touches-the-world-until-every-pure-check-passes 300
  (prop/for-all [tree (gen/one-of [gen-tree gen-ordered-tree])]
    (reset! ran [])
    (let [{:keys [failures]} (outcome #(pt/parse tree))
          pure-failed?       (not (every? #(and (int? (:v %)) (every? (fn [k] (and (int? (:v k)) (< (:v k) (:v %))))
                                                                       (:kids %)))
                                          (nodes-of tree)))]
      (or (not pure-failed?)
          (and (some? failures)
               (not-any? #(#{::v-known ::v-row} (if (vector? %) (first %) %)) @ran))))))

(defspec a-parsed-tree-holds-parsed-nodes-and-fetches-once 300
  (prop/for-all [tree gen-ordered-tree]
    (reset! ran [])
    (let [result (outcome #(pt/parse tree))
          calls  (filterv #(and (vector? %) (= ::v-row (first %))) @ran)
          vs     (into [] (comp (map (fn [n] [(:v n)])) (distinct)) (nodes-of tree))]
      (and (= [[::v-row (sort vs)]] (mapv (fn [[n ts]] [n (sort ts)]) calls))
           (if (:ok result)
             (let [parsed (pt/parse tree)]
               (and (every? #(pt/parsed? Node %) (nodes-of parsed))
                    (= (map :v (nodes-of tree)) (map :v (nodes-of parsed)))))
             (some #(= 13 (:v %)) (nodes-of tree)))))))

(defn- fresh-tree
  "The same tree, every node a new draft."
  [node]
  (->NodeDraft (:v node) (mapv fresh-tree (:kids node))))

(defspec parse-all-agrees-with-parse-on-trees 200
  (prop/for-all [trees (gen/vector (gen/one-of [gen-tree gen-ordered-tree]) 0 5)]
    (let [each     (mapv #(outcome (fn [] (pt/parse (fresh-tree %)))) trees)
          all      (outcome #(pt/parse-all (mapv fresh-tree trees)))
          failures (into (sorted-map) (keep-indexed (fn [i o] (when-not (:ok o) [i (:failures o)]))) each)]
      (if (empty? failures)
        (= all {:ok (mapv :ok each)})
        (= all {:type         ::pt/invalid
                :failed-kinds (apply set/union (keep :failed-kinds each))
                :failures     failures})))))

(defspec draft-of-round-trips-a-tree 200
  (prop/for-all [tree gen-ordered-tree]
    (let [result (outcome #(pt/parse tree))]
      (or (not (:ok result))
          (let [parsed (pt/parse tree)
                again  (pt/parse (pt/draft-of parsed))]
            (and (= (map :v (nodes-of parsed)) (map :v (nodes-of again)))
                 (every? #(pt/parsed? Node %) (nodes-of again))))))))

;;; Sums

(pt/defsum TripleOrPair [Triple PosPair])

(defn- which [x]
  (pt/match TripleOrPair x
    Triple  t [:triple (:a t)]
    PosPair p [:pair (:a p)]))

(def ^:private gen-candidate
  "A parsed member, a forged member, a draft, or a parsed value of another type."
  (gen/one-of [(gen/fmap (fn [d] [:parsed d]) gen-valid-draft)
               (gen/fmap (fn [a] [:parsed (->PosPairDraft a)]) (gen/fmap inc gen/nat))
               (gen/fmap (fn [d] [:forged d]) gen-any-draft)
               (gen/fmap (fn [a] [:draft (->PosPairDraft a)]) gen/small-integer)
               (gen/fmap (fn [v] [:other (->NodeDraft v [])]) gen/small-integer)]))

(defspec match-runs-only-on-parsed-members 300
  (prop/for-all [[kind d] gen-candidate]
    (let [x      (case kind
                   :parsed (pt/parse d)
                   :forged (->Triple (:user-id d) (:a d) (:b d) (:c d))
                   :draft  d
                   :other  (try (pt/parse d) (catch clojure.lang.ExceptionInfo _ d)))
          result (try (which x) (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
      (if (= :parsed kind)
        (= [(if (instance? Triple x) :triple :pair) (:a x)] result)
        (= ::pt/not-parsed result)))))
