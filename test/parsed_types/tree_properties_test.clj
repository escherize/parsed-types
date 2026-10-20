(ns parsed-types.tree-properties-test
  "Generative tests of tree parsing against a model written by hand: the whole failure map (paths, :returned,
  :blocked-by, :blocked) and the kinds that failed, for random trees with :one and :many children, malformed children, a node
  that does not read its child, a fetch and checks that read child fields, and edits to child fields."
  (:require
   [clojure.set :as set]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

(def ^:private seen (atom []))

(defn- leaf-rows [tuples]
  (swap! seen conj [::leaf-row tuples])
  (into {} (map (fn [[n :as t]] [t {:n n}])) tuples))

(declare views?)

(defn- crate-rows [tuples]
  (swap! seen conj [::crate-row (mapv first tuples) (every? (fn [[_ boxes]] (views? boxes)) tuples)])
  (into {} (map (fn [[_ boxes :as t]] [t {:box-count (count boxes)}])) tuples))

(pt/defparsed Leaf [n]
  (pure ::leaf-int "n is an integer." [n] (int? n))
  (external ::leaf-known "n is not 13." [] [row ::leaf-row] (not= 13 (:n row)))
  (fetch ::leaf-row [n] leaf-rows))

(pt/defparsed Box [label item]
  (child item Leaf)
  (pure ::label-kw "The label is a keyword." [label] (keyword? label)))

(pt/defparsed Crate [total boxes leaves]
  (children boxes Box)
  (children leaves Leaf)
  (pure ::total-int "The total is an integer." [total] (int? total))
  (pure ::sum-fits "The leaves add up to at most the total." [total leaves]
        (<= (reduce + 0 (map :n leaves)) total))
  (pure ::boxes-labeled "Every box has a label." [boxes] (every? #(keyword? (:label %)) boxes))
  (external ::few-boxes "At most 3 boxes." [] [row ::crate-row] (<= (:box-count row) 3))
  (external ::views-seen "The checks see children as values, not drafts." [boxes] (views? boxes))
  (fetch ::crate-row [total boxes] crate-rows))

(defn- views?
  "Whether `boxes` holds values the checks may read: no draft anywhere in it."
  [boxes]
  (not-any? #(or (instance? BoxDraft %) (instance? LeafDraft (:item %))) boxes))

;;; Generators

(def ^:private gen-n (gen/frequency [[6 (gen/choose 0 15)] [1 (gen/return nil)] [1 (gen/return "x")]]))

(def ^:private gen-leaf (gen/fmap ->LeafDraft gen-n))

(def ^:private gen-item
  (gen/frequency [[3 gen-leaf] [2 (gen/return nil)]
                  [1 (gen/return (->BoxDraft :wrong nil))] [1 (gen/return "junk")]]))

(def ^:private gen-box
  (gen/fmap (fn [[label item]] (->BoxDraft label item))
            (gen/tuple (gen/frequency [[6 gen/keyword] [1 (gen/return nil)] [1 (gen/return 1)]]) gen-item)))

(defn- gen-children [gen-good gen-wrong]
  (gen/frequency [[8 (gen/vector (gen/frequency [[8 gen-good] [1 gen-wrong]]) 0 4)]
                  [1 (gen/return nil)]
                  [1 (gen/fmap #(apply list %) (gen/vector gen-good 1 2))]]))

(def ^:private gen-crate
  (gen/fmap (fn [[total boxes leaves]] (->CrateDraft total boxes leaves))
            (gen/tuple (gen/frequency [[6 (gen/choose 0 40)] [1 (gen/return nil)]])
                       (gen-children gen-box gen-leaf)
                       (gen-children gen-leaf (gen/return (->BoxDraft :wrong nil))))))

(def ^:private gen-well-formed-crate
  "A crate whose every pure check passes, so the external checks decide: a leaf of 13 fails, and so do more than 3
  boxes."
  (gen/fmap (fn [[labels items ns]]
              (->CrateDraft 1000
                            (mapv (fn [l it] (->BoxDraft l (some-> it ->LeafDraft))) labels items)
                            (mapv ->LeafDraft ns)))
            (gen/bind (gen/choose 0 5)
                      (fn [k] (gen/tuple (gen/vector gen/keyword k)
                                         (gen/vector (gen/one-of [(gen/return nil) (gen/choose 0 15)]) k)
                                         (gen/vector (gen/choose 0 15) 0 4))))))

(def ^:private gen-any-crate
  (gen/one-of [gen-crate gen-well-formed-crate]))

;;; The model

(defn- leaf? [x] (instance? LeafDraft x))
(defn- box? [x] (instance? BoxDraft x))

(defn- fail
  ([doc] {:doc doc, :returned false})
  ([doc held-by] {:doc doc, :blocked-by (set held-by)}))

(defn- node
  "One node's pure phase: its failures in run order as `[name entry kind]`, the checks each failure holds up, and
  its externals."
  [path failures blocks externals]
  {:path path, :failures failures, :blocks blocks, :externals externals})

(declare model-nodes)

(defn- leaf-nodes [path {:keys [n]}]
  [(node path
         (when-not (int? n) [[::leaf-int (fail "n is an integer.") :pure]])
         {::leaf-int #{}}
         [[::leaf-known (fail "n is not 13.") :external (not= 13 n)]])])

(defn- box-nodes [path {:keys [label item]}]
  (let [shape-ok? (or (nil? item) (leaf? item))
        kids      (when (and shape-ok? item) (leaf-nodes (conj path :item) item))]
    (conj (vec kids)
          (node path
                (cond-> []
                  (not shape-ok?)      (conj [:parsed-types.child/item
                                              (fail "item holds nil or a draft that parses to Leaf.") :pure])
                  (not (keyword? label)) (conj [::label-kw (fail "The label is a keyword.") :pure]))
                {}
                []))))

(defn- bad-paths
  "The paths of the direct children whose subtree has a failure."
  [nodes child-paths]
  (filter (fn [p] (some #(and (seq (:failures %)) (= p (take (count p) (:path %)))) nodes)) child-paths))

(defn- crate-nodes [{:keys [total boxes leaves]}]
  (let [boxes-ok?  (and (vector? boxes) (every? box? boxes))
        leaves-ok? (and (vector? leaves) (every? leaf? leaves))
        box-kids   (when boxes-ok? (vec (mapcat (fn [i b] (box-nodes [:boxes i] b)) (range) boxes)))
        leaf-kids  (when leaves-ok? (vec (mapcat (fn [i l] (leaf-nodes [:leaves i] l)) (range) leaves)))
        kids       (into (vec box-kids) leaf-kids)
        bad-boxes  (when boxes-ok? (bad-paths kids (map (fn [i] [:boxes i]) (range (count boxes)))))
        bad-leaves (when leaves-ok? (bad-paths kids (map (fn [i] [:leaves i]) (range (count leaves)))))
        total-ok?  (int? total)
        failures   (cond-> []
                     (not boxes-ok?)  (conj [:parsed-types.child/boxes
                                             (fail "boxes holds a vector of drafts that parse to Box.") :pure])
                     (not leaves-ok?) (conj [:parsed-types.child/leaves
                                             (fail "leaves holds a vector of drafts that parse to Leaf.") :pure])
                     (not total-ok?)  (conj [::total-int (fail "The total is an integer.") :pure])
                     (and boxes-ok? (seq bad-boxes))
                     (conj [::boxes-labeled (fail "Every box has a label." bad-boxes) :pure])
                     (and total-ok? leaves-ok? (seq bad-leaves))
                     (conj [::sum-fits (fail "The leaves add up to at most the total." bad-leaves) :pure])
                     (and total-ok? leaves-ok? (empty? bad-leaves) (< total (reduce + 0 (map :n leaves))))
                     (conj [::sum-fits (fail "The leaves add up to at most the total.") :pure]))]
    (conj kids
          (node []
                failures
                {:parsed-types.child/boxes  #{::boxes-labeled}
                 :parsed-types.child/leaves #{::sum-fits}
                 ::total-int                #{::sum-fits}}
                [[::few-boxes (fail "At most 3 boxes.") :external (<= (count boxes) 3)]
                 [::views-seen (fail "The checks see children as values, not drafts.") :external true]]))))

(defn- entry [e] (update e :blocked-by #(when % (set %))))

(defn- model
  "`{:failed-kinds :failures}`, the failures as `{[path check] entry}`, or `:ok`. A held check failed nothing itself,
  so its kind is not counted."
  [crate]
  (let [nodes    (crate-nodes crate)
        failing  (filter (comp seq :failures) nodes)]
    (if (seq failing)
      {:failed-kinds (into #{} (for [{:keys [failures]} failing
                                     [_ e kind]         failures
                                     :when              (contains? e :returned)]
                                 kind))
       :failures (into {}
                       (for [{:keys [path failures blocks externals]} failing
                             :let  [failed (set (map first failures))]
                             [n e] failures]
                         [[path n]
                          (assoc (entry e) :blocked
                                 (into (set (remove failed (get blocks n #{}))) (map first externals)))]))}
      (let [ext-failures (for [{:keys [path externals]} nodes
                               [n e kind ok?] externals
                               :when (not ok?)]
                           [path n e kind])
            ;; the crate's ::views-seen reads boxes, so it is held when a box, or its item, failed an external check
            bad-boxes    (filter (fn [bp] (some (fn [[path]] (= bp (take (count bp) path))) ext-failures))
                                 (map (fn [i] [:boxes i]) (range (count (:boxes crate)))))
            held         (when (seq bad-boxes)
                           [[[] ::views-seen] {:doc        "The checks see children as values, not drafts."
                                               :blocked-by (set bad-boxes)
                                               :blocked    #{}}])]
        (if (seq ext-failures)
          {:failed-kinds (into #{} (map #(nth % 3)) ext-failures)
           :failures (into {} (concat (for [[path n e] ext-failures] [[path n] (assoc (entry e) :blocked #{})])
                                      (when held [held])))}
          :ok)))))

;;; What parse returned, in the model's shape

(defn- flat-failures [m path]
  (reduce-kv (fn [acc k v]
               (cond
                 (qualified-keyword? k)
                 (assoc acc [path k] {:doc        (:doc v)
                                      :returned   (:returned v)
                                      :blocked-by (some-> (:blocked-by v) set)
                                      :blocked    (set (:blocked v))})

                 (every? int? (keys v))
                 (reduce-kv (fn [acc i sub] (merge acc (flat-failures sub (conj path k i)))) acc v)

                 :else
                 (merge acc (flat-failures v (conj path k)))))
             {}
             m))

(defn- normalize-model [{:keys [failed-kinds failures]}]
  {:type         ::pt/invalid
   :failed-kinds failed-kinds
   :failures (into {} (map (fn [[k e]] [k (merge {:returned nil, :blocked-by nil} e)])) failures)})

(defn- parse-outcome [crate]
  (try (pt/parse crate)
       :ok
       (catch clojure.lang.ExceptionInfo e
         (let [{:keys [type failed-kinds failures]} (ex-data e)]
           {:type type, :failed-kinds failed-kinds, :failures (flat-failures failures [])}))))

(defn- all-parsed?
  "Whether every node of a parsed crate is a parsed value of its type, :one children included."
  [c]
  (and (pt/parsed? Crate c)
       (every? #(and (pt/parsed? Box %) (or (nil? (:item %)) (pt/parsed? Leaf (:item %)))) (:boxes c))
       (every? #(pt/parsed? Leaf %) (:leaves c))))

(defn- fresh
  "The same tree, every draft new."
  [x]
  (cond
    (instance? CrateDraft x) (->CrateDraft (:total x) (fresh (:boxes x)) (fresh (:leaves x)))
    (box? x)                 (->BoxDraft (:label x) (fresh (:item x)))
    (leaf? x)                (->LeafDraft (:n x))
    (vector? x)              (mapv fresh x)
    (seq? x)                 (apply list (map fresh x))
    :else                    x))

;;; Properties

(defspec parse-matches-the-tree-model 500
  (prop/for-all [crate gen-any-crate]
    (let [expected (model crate)
          actual   (parse-outcome crate)]
      (if (= :ok expected)
        (and (= :ok actual) (all-parsed? (pt/parse (fresh crate))))
        (= (normalize-model expected) actual)))))

(defspec the-tree-generator-reaches-every-outcome 1
  (prop/for-all [crates (gen/vector gen-any-crate 500)]
    (let [models (map model crates)]
      (and (= #{#{:pure} #{:external} :ok} (set (map #(if (= :ok %) :ok (:failed-kinds %)) models)))
           ;; every check fails in some crate
           (set/subset? #{::leaf-int ::leaf-known ::label-kw ::total-int ::sum-fits ::boxes-labeled ::few-boxes
                          :parsed-types.child/item :parsed-types.child/boxes :parsed-types.child/leaves}
                        (set (mapcat #(map second (keys (:failures %))) (remove #{:ok} models))))))))

(defspec nothing-in-a-failing-tree-fetches 300
  (prop/for-all [crate gen-any-crate]
    (reset! seen [])
    (let [expected (model crate)]
      (parse-outcome crate)
      (or (= :ok expected)
          (not (contains? (:failed-kinds expected) :pure))
          (empty? @seen)))))

(defspec fetches-see-checked-values-and-run-once 300
  (prop/for-all [crate gen-any-crate]
    (reset! seen [])
    (parse-outcome crate)
    (let [crate-calls (filter #(= ::crate-row (first %)) @seen)
          leaf-calls  (filter #(= ::leaf-row (first %)) @seen)]
      (and (<= (count crate-calls) 1)
           (<= (count leaf-calls) 1)
           (every? #(true? (nth % 2)) crate-calls)))))

(def ^:private gen-edit
  "An edit to a crate: a new total, a leaf replaced, a box's item replaced, or a draft-of round trip."
  (gen/one-of [(gen/fmap (fn [t] [:total t]) (gen/choose 0 40))
               (gen/fmap (fn [[i l]] [:leaf i l]) (gen/tuple gen/nat gen-leaf))
               (gen/fmap (fn [[i it]] [:item i it]) (gen/tuple gen/nat gen-item))
               (gen/return [:round-trip])]))

(defn- edit [crate [op x y]]
  (case op
    :total      (assoc crate :total x)
    :leaf       (if (and (vector? (:leaves crate)) (seq (:leaves crate)))
                  (assoc-in crate [:leaves (mod x (count (:leaves crate)))] y)
                  crate)
    :item       (let [boxes (:boxes crate)]
                  (if (and (vector? boxes) (seq boxes) (box? (nth boxes (mod x (count boxes)))))
                    (assoc-in crate [:boxes (mod x (count boxes)) :item] y)
                    crate))
    :round-trip (try (pt/draft-of (pt/parse crate)) (catch clojure.lang.ExceptionInfo _ crate))))

(defspec edits-to-child-fields-match-the-tree-model 300
  (prop/for-all [crate gen-any-crate
                 edits (gen/vector gen-edit 0 6)]
    (let [edited   (reduce edit crate edits)
          expected (model edited)
          actual   (parse-outcome edited)]
      (if (= :ok expected)
        (= :ok actual)
        (= (normalize-model expected) actual)))))

(defspec parse-all-agrees-with-parse-on-trees 200
  (prop/for-all [crates (gen/vector gen-any-crate 0 4)]
    (let [each (mapv #(parse-outcome (fresh %)) crates)
          all  (try (pt/parse-all (mapv fresh crates)) :ok
                    (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (if (every? #{:ok} each)
        (= :ok all)
        (and (= ::pt/invalid (:type all))
             (= (apply set/union (keep :failed-kinds each)) (:failed-kinds all))
             (= (into {} (keep-indexed (fn [i o] (when-not (= :ok o) [i (:failures o)]))) each)
                (into {} (map (fn [[i f]] [i (flat-failures f [])])) (:failures all))))))))

(defspec draft-of-returns-drafts-all-the-way-down 200
  (prop/for-all [crate (gen/such-that #(= :ok (model %)) gen-well-formed-crate 100)]
    (let [parsed (pt/parse crate)
          back   (pt/draft-of parsed)]
      (and (instance? CrateDraft back)
           (every? #(and (box? %) (or (nil? (:item %)) (leaf? (:item %)))) (:boxes back))
           (every? leaf? (:leaves back))
           (= :ok (parse-outcome back))))))
