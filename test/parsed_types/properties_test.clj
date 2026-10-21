(ns parsed-types.properties-test
  "Generative tests of the properties the library promises, over random inputs and random sequences of edits."
  (:require
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
  ;; written after ::b-string, so it runs only when that passes
  (pure ::b-short "b has at most 3 characters."
        [b]
        (log! ::b-short)
        (or (<= (count (str b)) 3) {:length (count (str b))}))
  (pure ::a-below-c "a is below c." [a c] (log! ::a-below-c) (< a c))
  (pure ::user-3-c-even "User 3's c is even." [user-id c] (log! ::user-3-c-even) (or (not= 3 user-id) (even? c)))
  (external ::may-use-row "User 1 may use the row for a."
            [user-id] [row ::row]
            (log! ::may-use-row)
            (and (= 1 user-id) (:ok? row)))
  (external ::user-known "The user is known. An external check with no fetch."
            [user-id]
            (log! ::user-known)
            (contains? #{1 2} user-id))
  ;; written last, so it runs only when every check before it passes
  (external ::row-small "The row for a is small."
            [] [row ::row]
            (log! ::row-small)
            (true? (:small? row)))
  (fetch ::row [a] rows))

(def ^:private fields [:user-id :a :b :c])

(def ^:private written-order
  "The checks of Triple, pure then external, each in the order written."
  [::a-int ::b-string ::c-int ::b-short ::a-below-c ::user-3-c-even ::may-use-row ::user-known ::row-small])

(def ^:private touches-world? #{::may-use-row ::user-known ::row-small ::row})

(def ^:private gen-value
  (gen/one-of [gen/small-integer gen/small-integer gen/string-alphanumeric (gen/return nil)]))

(def ^:private gen-any-fields
  (gen/fmap (fn [[u a b c]] {:user-id u, :a a, :b b, :c c})
            (gen/tuple (gen/elements [1 2 3]) gen-value gen-value gen-value)))

(def ^:private gen-well-formed-fields
  "Fields that pass every pure check, and any user and parity, so the external checks fail about half the time."
  (gen/fmap (fn [[u a b gap]] {:user-id u, :a a, :b (subs b 0 (min 3 (count b))), :c (+ a gap 1)})
            (gen/tuple (gen/elements [1 1 2 3]) gen/small-integer gen/string-alphanumeric gen/nat)))

(def ^:private gen-valid-fields
  "Fields that pass every check: user 1, an even a between -10 and 10, a short b, and c above a."
  (gen/fmap (fn [[half b gap]] {:user-id 1, :a (* 2 (rem half 5)), :b (subs b 0 (min 3 (count b))), :c (+ (* 2 (rem half 5)) gap 1)})
            (gen/tuple gen/small-integer gen/string-alphanumeric gen/nat)))

(def ^:private gen-fields
  "Half malformed and half well-formed, so the pure, the external and the passing paths are all exercised."
  (gen/one-of [gen-any-fields gen-well-formed-fields]))

(defn- read-parsed
  "The fields of `x`, a parsed value of any type, through pt/parsed."
  [x]
  (pt/parsed (class x) x))

(defn- outcome
  "What `thunk` returned, as a plain value, or the ex-data it threw, without the exception a check threw."
  [thunk]
  (try
    (let [x (thunk)]
      {:ok (if (vector? x) (mapv read-parsed x) (read-parsed x))})
    (catch clojure.lang.ExceptionInfo e
      (cond-> (dissoc (ex-data e) :threw)
        (contains? (ex-data e) :threw) (assoc :threw (class (:threw (ex-data e))))))))

(defspec nothing-touches-the-world-until-every-pure-check-passes 300
  (prop/for-all [m gen-fields]
    (reset! ran [])
    (let [{:keys [kind]} (outcome #(pt/parse Triple m))]
      (or (not= :pure kind)
          (not-any? #(touches-world? (if (vector? %) (first %) %)) @ran)))))

(defspec nothing-runs-after-the-first-check-that-fails 300
  (prop/for-all [m gen-fields]
    (reset! ran [])
    (let [{:keys [check ok]} (outcome #(pt/parse Triple m))
          checks-ran         (remove vector? @ran)]
      (= (if ok
           written-order
           (conj (vec (take-while #(not= check %) written-order)) check))
         checks-ran))))

(defspec parse-issues-a-new-equal-value-and-nothing-else-is-parsed 300
  (prop/for-all [m gen-fields]
    (let [forged (Triple. (:user-id m) (:a m) (:b m) (:c m) nil)
          result (outcome #(pt/parse Triple m))]
      (and (not (pt/parsed? Triple forged))
           (not (pt/parsed? Triple m))
           (if (:ok result)
             (let [parsed (pt/parse Triple m)]
               (and (pt/parsed? Triple parsed)
                    (= (pt/parsed Triple parsed) m)
                    (not= parsed m)
                    (not= m parsed)
                    (= parsed (pt/parse Triple m))
                    (not= parsed forged)
                    (not= forged parsed)
                    (not (identical? parsed forged))))
             true)))))

(defspec a-parsed-value-cannot-be-changed 200
  (prop/for-all [m gen-valid-fields
                 k (gen/one-of [(gen/elements fields) gen/keyword])
                 v gen-value]
    (let [parsed  (pt/parse Triple m)
          ;; a key that is not a field is reported as that first
          as      (if (some #{k} fields) ::pt/cannot-change ::pt/no-field)
          refused (fn [thunk type]
                    (try (thunk) false
                         (catch clojure.lang.ExceptionInfo e (= type (:type (ex-data e))))))]
      (and (refused #(assoc parsed k v) as)
           ;; merge is conj, which names no key
           (refused #(merge parsed {k v}) ::pt/cannot-change)
           (refused #(get parsed k) ::pt/must-unwrap)
           (refused #(with-meta parsed {k v}) ::pt/cannot-change)
           (pt/parsed? Triple parsed)))))

(defspec parsed-fields-round-trip 200
  (prop/for-all [m gen-valid-fields]
    (let [parsed   (pt/parse Triple m)
          reparsed (pt/parse Triple (pt/parsed Triple parsed))]
      (and (= parsed reparsed)
           (not (identical? parsed reparsed))
           (pt/parsed? Triple reparsed)))))

(defn- parse-all-from-parse
  "What parse-all must return, from what parse returned for each map: every value when every map parsed; else the
  failure of the first map whose pure checks failed, as every map's pure checks run before any external check;
  else the failure of the first map whose external checks failed. The failure carries its map's index."
  [each]
  (let [first-of (fn [kind] (first (keep-indexed (fn [i o] (when (= kind (:kind o)) (assoc o :index i))) each)))]
    (or (first-of :pure)
        (first-of :external)
        {:ok (mapv :ok each)})))

(defspec parse-all-agrees-with-parse 300
  (prop/for-all [ms (gen/vector gen-fields 0 8)]
    (= (parse-all-from-parse (mapv #(outcome (fn [] (pt/parse Triple %))) ms))
       (outcome #(pt/parse-all Triple ms)))))

(declare model)

(defspec parse-all-fetches-once-with-each-distinct-tuple 300
  (prop/for-all [ms (gen/vector gen-fields 0 8)]
    (let [pure-ok? (fn [m] (not= :pure (:kind (model m))))
          ;; a pure failure in any map throws before anything is fetched
          expected (if (every? pure-ok? ms) (into [] (comp (map (fn [m] [(:a m)])) (distinct)) ms) [])]
      (reset! ran [])
      (outcome #(pt/parse-all Triple ms))
      (let [calls (filterv #(and (vector? %) (= ::row (first %))) @ran)]
        (if (empty? expected)
          (empty? calls)
          (= [[::row expected]] calls))))))

;;; A reference model: what parse must return, written out by hand from the declaration, so a bug the library makes on
;;; both sides of a comparison still fails here.

(defn- model
  "The outcome of parsing a Triple from these fields, from first principles: the first check, pure then external,
  each in the order written, that does not return exactly true, or every field when none fails. Each check is a fn, so
  a check after the first failure is never called, as parse never runs it."
  [{:keys [user-id a b c], :as fields}]
  (let [checks [[:pure ::a-int "a is an integer." #(int? a)]
                [:pure ::b-string "b is a string." #(string? b)]
                [:pure ::c-int "c is an integer." #(int? c)]
                [:pure ::b-short "b has at most 3 characters." #(or (<= (count b) 3) {:length (count b)})]
                [:pure ::a-below-c "a is below c." #(< a c)]
                [:pure ::user-3-c-even "User 3's c is even." #(or (not= 3 user-id) (even? c))]
                ;; the row for a is {:ok? (even? a), :small? (< -10 a 10)}
                [:external ::may-use-row "User 1 may use the row for a." #(and (= 1 user-id) (even? a))]
                [:external ::user-known "The user is known. An external check with no fetch." #(contains? #{1 2} user-id)]
                [:external ::row-small "The row for a is small." #(< -10 a 10)]]]
    (or (some (fn [[kind n doc f]]
                (let [r (f)]
                  (when-not (true? r)
                    (cond-> {:type     ::pt/invalid
                             :rule     :parsed-types.rule/check-failed
                             :check    n
                             :doc      doc
                             :kind     kind
                             :parsed   "parsed_types.properties_test.Triple"}
                      ;; never what an external check returned
                      (= :pure kind) (assoc :returned r)))))
              checks)
        {:ok (into {} fields)})))

(defspec parse-matches-the-model 500
  (prop/for-all [m gen-fields]
    (= (model m) (outcome #(pt/parse Triple m)))))

(defspec the-generators-reach-every-outcome 1
  (prop/for-all [ms (gen/vector gen-fields 400)]
    (= #{:pure :external :ok} (set (map #(or (:kind %) :ok) (map model ms))))))

;;; Two declarations that share a fetch name with different fetches

(defn- pos-rows [tuples] (into {} (map (fn [[a :as t]] [t {:ok? (and (int? a) (pos? a))}])) tuples))

(defn- neg-rows [tuples] (into {} (map (fn [[a :as t]] [t {:ok? (and (int? a) (neg? a))}])) tuples))

;; Both fetch ::row, differently.
(pt/defparsed PosPair [a]
  (external ::pair-ok "The row for a is ok." [] [row ::row] (true? (:ok? row)))
  (fetch ::row [a] pos-rows))

(pt/defparsed NegPair [a]
  (external ::pair-ok "The row for a is ok." [] [row ::row] (true? (:ok? row)))
  (fetch ::row [a] neg-rows))

(defspec each-declaration-uses-its-own-fetch 300
  (prop/for-all [as (gen/vector gen/small-integer 0 8)]
    (let [ms (mapv (fn [a] {:a a}) as)]
      (and (= (parse-all-from-parse (mapv #(outcome (fn [] (pt/parse PosPair %))) ms))
              (outcome #(pt/parse-all PosPair ms)))
           (= (parse-all-from-parse (mapv #(outcome (fn [] (pt/parse NegPair %))) ms))
              (outcome #(pt/parse-all NegPair ms)))))))

;;; Sums

(pt/defsum TripleOrPair [Triple PosPair])

(defn- which [x]
  (pt/match TripleOrPair x
    Triple  t [:triple (:a (pt/parsed Triple t))]
    PosPair p [:pair (:a (pt/parsed PosPair p))]))

(def ^:private gen-candidate
  "A parsed member, a forged member, a plain map of a member's fields, or a parsed value of another type: `[kind
  type-or-nil fields]`."
  (gen/one-of [(gen/fmap (fn [m] [:parsed Triple m]) gen-valid-fields)
               (gen/fmap (fn [a] [:parsed PosPair {:a a}]) (gen/fmap inc gen/nat))
               (gen/fmap (fn [m] [:forged nil m]) gen-any-fields)
               (gen/fmap (fn [a] [:plain nil {:a a}]) gen/small-integer)
               (gen/fmap (fn [a] [:other NegPair {:a a}]) (gen/fmap #(- -1 %) gen/nat))]))

(defspec match-runs-only-on-parsed-members 300
  (prop/for-all [[kind t m] gen-candidate]
    (let [x      (case kind
                   :parsed (pt/parse t m)
                   :forged (Triple. (:user-id m) (:a m) (:b m) (:c m) nil)
                   :plain  m
                   :other  (pt/parse t m))
          result (try (which x) (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))]
      (if (= :parsed kind)
        (= [(if (instance? Triple x) :triple :pair) (:a (read-parsed x))] result)
        (= ::pt/not-parsed result)))))
