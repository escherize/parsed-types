(ns parsed-types.properties-test
  "Generative tests of the properties the library promises, over random drafts and random sequences of edits."
  (:require
   [clojure.set :as set]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;; What ran, in order.
(def ^:private ran (atom []))

(defn- log! [x] (swap! ran conj x) true)

(defn- rows [arg-tuples]
  (log! [::row arg-tuples])
  (into {} (map (fn [[a :as t]] [t {:ok? (even? a), :small? (< -10 a 10)}])) arg-tuples))

(pt/defparsed Triple [user-id a b c]
  (pure ::a-int "a is an integer." [a] (log! ::a-int) (int? a))
  (pure ::b-string "b is a string." [b] (log! ::b-string) (string? b))
  (pure ::c-int "c is an integer." [c] (log! ::c-int) (int? c))
  ;; written after ::b-string, which reads b too, so it runs only when that passes
  (pure ::b-short "b has at most 3 characters."
        [b]
        (log! ::b-short)
        (or (<= (count (str b)) 3) {:length (count (str b))}))
  (pure ::a-below-c "a is below c." [a c] (log! ::a-below-c) (< a c))
  ;; shares c with ::c-int and ::a-below-c, and nothing with ::a-int: held only when one of those two ran and failed
  (pure ::user-3-c-even "User 3's c is even." [user-id c] (log! ::user-3-c-even) (or (not= 3 user-id) (even? c)))
  (external ::may-use-row "User 1 may use the row for a."
            [user-id] [row ::row]
            (log! ::may-use-row)
            (and (= 1 user-id) (:ok? row)))
  (external ::user-known "The user is known. An external check with no fetch."
            [user-id]
            (log! ::user-known)
            (contains? #{1 2} user-id))
  ;; binds ::row, as ::may-use-row does, so it runs only when that passes; ::user-known binds no fetch and holds nothing
  (external ::row-small "The row for a is small."
            [] [row ::row]
            (log! ::row-small)
            (true? (:small? row)))
  (fetch ::row [a] rows))

(def ^:private fields [:user-id :a :b :c])

(def ^:private pure-names #{::a-int ::b-string ::c-int ::b-short ::a-below-c ::user-3-c-even})

(def ^:private touches-world? #{::may-use-row ::user-known ::row-small ::row})

(def ^:private gen-value
  (gen/one-of [gen/small-integer gen/small-integer gen/string-alphanumeric (gen/return nil)]))

(def ^:private gen-any-draft
  (gen/fmap (fn [[u a b c]] (pt/draft Triple {:user-id u, :a a, :b b, :c c}))
            (gen/tuple (gen/elements [1 2 3]) gen-value gen-value gen-value)))

(def ^:private gen-well-formed-draft
  "A draft that passes every pure check, and any user and parity, so the external checks fail about half the time."
  (gen/fmap (fn [[u a b gap]] (pt/draft Triple {:user-id u, :a a, :b (subs b 0 (min 3 (count b))), :c (+ a gap 1)}))
            (gen/tuple (gen/elements [1 1 2 3]) gen/small-integer gen/string-alphanumeric gen/nat)))

(def ^:private gen-valid-draft
  "A draft that passes every check: user 1, an even a between -10 and 10, a short b, and c above a."
  (gen/fmap (fn [[half b gap]] (pt/draft Triple {:user-id 1, :a (* 2 (rem half 5)), :b (subs b 0 (min 3 (count b))), :c (+ (* 2 (rem half 5)) gap 1)}))
            (gen/tuple gen/small-integer gen/string-alphanumeric gen/nat)))

(def ^:private gen-draft
  "Half malformed and half well-formed, so the pure, the external and the passing paths are all exercised."
  (gen/one-of [gen-any-draft gen-well-formed-draft]))

(defn- fresh
  "A new draft with the same fields as `draft`."
  [draft]
  (pt/draft Triple {:user-id (:user-id draft), :a (:a draft), :b (:b draft), :c (:c draft)}))

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
    (let [forged (Triple. (:user-id draft) (:a draft) (:b draft) (:c draft) nil)
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
                    (= parsed (pt/parse (fresh draft)))
                    (not= parsed forged)
                    (not= forged parsed)
                    (not (identical? parsed forged))))
             true)))))

(defspec a-parsed-value-cannot-be-changed 200
  (prop/for-all [draft gen-valid-draft
                 k     (gen/one-of [(gen/elements fields) gen/keyword])
                 v     gen-value]
    (let [parsed  (pt/parse draft)
          ;; a key that is not a field is reported as that first
          as      (if (some #{k} fields) ::pt/cannot-change ::pt/no-field)
          refused (fn [thunk type]
                    (try (thunk) false
                         (catch clojure.lang.ExceptionInfo e (= type (:type (ex-data e))))))]
      (and (refused #(assoc parsed k v) as)
           (refused #(dissoc parsed k) as)
           (refused #(merge parsed {k v}) as)
           (refused #(with-meta parsed {k v}) ::pt/cannot-change)
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

(declare model)

(defspec parse-all-fetches-once-with-each-distinct-tuple 300
  (prop/for-all [drafts (gen/vector gen-draft 0 8)]
    (let [pure-ok? (fn [d] (not= #{:pure} (:failed-kinds (model d))))
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
  (let [externals [::may-use-row ::user-known ::row-small]
        singles   (cond-> [[::a-int "a is an integer." (int? a)]
                           [::b-string "b is a string." (string? b)]
                           [::c-int "c is an integer." (int? c)]]
                    ;; a pure check is held when one written before it that shares a field failed
                    (string? b)
                    (conj [::b-short "b has at most 3 characters." (or (<= (count (str b)) 3) {:length (count (str b))})]))
        failed    (into {} (for [[n doc r] singles :when (not (true? r))] [n {:doc doc, :returned r}]))
        relation  (cond
                    (or (failed ::a-int) (failed ::c-int)) :blocked
                    (< a c)                                :passed
                    :else                                  :failed)
        ;; a held check did not run, so it holds nothing: ::a-below-c held by ::a-int does not hold ::user-3-c-even
        even-held (or (failed ::c-int) (= :failed relation))
        failed    (cond-> failed
                    (= :failed relation) (assoc ::a-below-c {:doc "a is below c.", :returned false})
                    (and (not even-held) (= 3 user-id) (not (even? c)))
                    (assoc ::user-3-c-even {:doc "User 3's c is even.", :returned false}))
        blocked   (fn [n] (vec (concat (when (= ::b-string n) [::b-short])
                                       (when (and (= :blocked relation) (#{::a-int ::c-int} n)) [::a-below-c])
                                       (when (#{::c-int ::a-below-c} n) [::user-3-c-even])
                                       externals)))]
    (if (seq failed)
      {:type         ::pt/invalid
       :failed-kinds #{:pure}
       :failures     (into {} (map (fn [[n e]] [n (assoc e :blocked (blocked n))])) failed)}
      (let [may-use? (and (= 1 user-id) (even? a))
            ext      (cond-> {}
                       ;; an external check is held when one written before it that binds its fetch failed
                       (not may-use?)
                       (assoc ::may-use-row {:doc "User 1 may use the row for a.", :returned false, :blocked [::row-small]})
                       (not (contains? #{1 2} user-id))
                       (assoc ::user-known {:doc "The user is known. An external check with no fetch.", :returned false})
                       (and may-use? (not (< -10 a 10)))
                       (assoc ::row-small {:doc "The row for a is small.", :returned false}))]
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
               (gen/fmap (fn [[positive? a]] (if positive? (pt/draft PosPair {:a a}) (pt/draft NegPair {:a a})))
                         (gen/tuple gen/boolean gen/small-integer))]))

(defspec parse-all-agrees-with-parse-across-declarations 300
  (prop/for-all [drafts (gen/vector gen-mixed-draft 0 8)]
    (let [fresh-any (fn [d] (cond
                              (instance? TripleDraft d)  (fresh d)
                              (instance? PosPairDraft d) (pt/draft PosPair {:a (:a d)})
                              :else                      (pt/draft NegPair {:a (:a d)})))
          each      (mapv #(outcome (fn [] (pt/parse (fresh-any %)))) drafts)
          all       (outcome #(pt/parse-all (mapv fresh-any drafts)))
          failures  (into (sorted-map) (keep-indexed (fn [i o] (when-not (:ok o) [i (:failures o)]))) each)]
      (if (empty? failures)
        (= all {:ok (mapv :ok each)})
        (= all {:type         ::pt/invalid
                :failed-kinds (apply set/union (keep :failed-kinds each))
                :failures     failures})))))

;;; Sums

(pt/defsum TripleOrPair [Triple PosPair])

(defn- which [x]
  (pt/match TripleOrPair x
    Triple  t [:triple (:a t)]
    PosPair p [:pair (:a p)]))

(def ^:private gen-candidate
  "A parsed member, a forged member, a draft, or a parsed value of another type."
  (gen/one-of [(gen/fmap (fn [d] [:parsed d]) gen-valid-draft)
               (gen/fmap (fn [a] [:parsed (pt/draft PosPair {:a a})]) (gen/fmap inc gen/nat))
               (gen/fmap (fn [d] [:forged d]) gen-any-draft)
               (gen/fmap (fn [a] [:draft (pt/draft PosPair {:a a})]) gen/small-integer)
               (gen/fmap (fn [a] [:other (pt/draft NegPair {:a a})]) (gen/fmap #(- -1 %) gen/nat))]))

(defspec match-runs-only-on-parsed-members 300
  (prop/for-all [[kind d] gen-candidate]
    (let [x      (case kind
                   :parsed (pt/parse d)
                   :forged (Triple. (:user-id d) (:a d) (:b d) (:c d) nil)
                   :draft  d
                   :other  (pt/parse d))
          result (try (which x) (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
      (if (= :parsed kind)
        (= [(if (instance? Triple x) :triple :pair) (:a x)] result)
        (= ::pt/not-parsed result)))))
