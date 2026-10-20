(ns parsed-types.defparsed-test
  "What `defparsed` refuses at compile time, and the declaration it writes."
  (:require
   [clojure.test :refer [deftest is testing]]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

(defn- compile-error
  "The message of the error compiling `form` in this namespace, or nil when it compiles."
  [form]
  (try (binding [*ns* (the-ns 'parsed-types.defparsed-test)] (eval form)) nil
       (catch Throwable e
         (ex-message (or (ex-cause e) e)))))

(deftest compile-errors-test
  (doseq [[label form re]
          [["a name that is not a symbol"
            '(pt/defparsed "A" [a])
            #"write \(defparsed Name \[fields\]"]
           ["a check that reads a field the draft does not have"
            '(pt/defparsed A [a] (pure ::c "d" [b] true))
            #"reads fields the draft does not have: \[b\]"]
           ["a check that uses a field it does not name"
            '(pt/defparsed A [a b] (pure ::c "d" [a] (< a b)))
            #"uses fields its argument vector does not name: \[b\]"]
           ["a check that uses a field named like a clojure.core fn, without naming it"
            '(pt/defparsed A [a name] (pure ::c "d" [a] (string? (name a))))
            #"uses fields its argument vector does not name: \[name\]"]
           ["a check that names a field twice"
            '(pt/defparsed A [a] (pure ::c "d" [a a] true))
            #"reads a field twice"]
           ["an unqualified check name"
            '(pt/defparsed A [a] (pure :c "d" [a] true))
            #"qualified keyword"]
           ["no doc"
            '(pt/defparsed A [a] (pure ::c [a] true))
            #"needs a doc string"]
           ["no body"
            '(pt/defparsed A [a] (pure ::c "d" [a]))
            #"needs a body"]
           ["a fetch that is not declared"
            '(pt/defparsed A [a] (external ::c "d" [a] [x ::nope] true))
            #"pairs of a local and a fetch declared"]
           ["a fetch over a field the draft does not have"
            '(pt/defparsed A [a] (fetch ::f [b] identity))
            #"fields of the draft"]
           ["a child field that is not a field"
            '(pt/defparsed A [a] (children b A))
            #"child field must be a field"]
           ["a name used twice"
            '(pt/defparsed A [a] (pure ::c "d" [a] true) (pure ::c "d" [a] true))
            #"used twice"]
           ["a field named twice"
            '(pt/defparsed A [a a])
            #"field is named twice"]
           ["a field declared as a child twice"
            '(pt/defparsed A [a] (child a A) (children a A))
            #"declared as a child twice"]
           ["a fetch bound to a field's name"
            '(pt/defparsed A [a] (external ::c "d" [] [a ::f] true) (fetch ::f [a] identity))
            #"fetch is bound to a field's name: \[a\]"]
           ["a pure check that binds a fetch"
            '(pt/defparsed A [a] (pure ::c "d" [a] [r ::f] true) (fetch ::f [a] identity))
            #"a pure check cannot fetch"]
           ["an unknown option"
            '(pt/defparsed A [a] (pure ::c "d" {:status-code 422} [a] true))
            #"unknown options \[:status-code\]"]
           ["a status that is not an integer"
            '(pt/defparsed A [a] (pure ::c "d" {:status "teapot"} [a] true))
            #":status must be an integer"]
           ["an unknown clause"
            '(pt/defparsed A [a] (check ::c "d" [a] true))
            #"a clause is \(pure"]]]
    (testing label
      (is (re-find re (str (compile-error form)))))))

(pt/defparsed Point [x y]
  (pure ::x-int "x is an integer." [x] (int? x))
  (pure ::y-int "y is an integer." {:status 422} [y] (int? y))
  (pure ::on-diagonal "x equals y." [x y] (= x y))
  (external ::known "The point is known." [] [row ::row] (some? row))
  (fetch ::row [x] (fn [tuples] (into {} (map (fn [t] [t :row])) tuples))))

(deftest declaration-test
  (testing "defparsed writes a plain declaration: the data you would write by hand"
    (let [decl (pt/declaration (->PointDraft 1 1))]
      (is (= Point (:parses-to decl)))
      (is (= [{:name ::x-int, :doc "x is an integer.", :kind :pure, :reads [:x]}
              {:name ::y-int, :doc "y is an integer.", :kind :pure, :reads [:y], :status-code 422}
              {:name ::on-diagonal, :doc "x equals y.", :kind :pure, :reads [:x :y]}
              {:name ::known, :doc "The point is known.", :kind :external, :reads [], :fetches [::row]}]
             (mapv #(dissoc % :f) (:checks decl))))
      (is (= [{:name ::row, :reads [:x]}] (mapv #(dissoc % :f) (:fetches decl))))))
  (testing "and parses with it"
    (is (= {:x 2, :y 2} (into {} (pt/parse (->PointDraft 2 2)))))
    (is (= 422 (:status-code (try (pt/parse (->PointDraft 1 :y)) (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(pt/defparsed-type Other [x z])

(pt/defsum Shape [Point Other])

(deftest match-compile-errors-test
  (doseq [[label form re]
          [["a member left out"
            '(pt/match Shape x Point p 1)
            #"does not handle \[\"Other\"\]"]
           ["a type that is not a member"
            '(pt/match Shape x Point p 1 Other o 2 String s 3)
            #"not members: \[\"String\"\]"]
           ["a member handled twice"
            '(pt/match Shape x Point p 1 Other o 2 Point q 3)
            #"handles a type twice"]
           ["not a sum"
            '(pt/match Point x Point p 1)
            #"is not a sum"]
           ["a clause that is not a triple"
            '(pt/match Shape x Point p)
            #"a type, a binding, and an expression"]]]
    (testing label
      (is (re-find re (str (compile-error form))))))
  (testing "a complete match compiles"
    (is (nil? (compile-error '(fn [x] (pt/match Shape x Point p (:x p) Other o (:x o))))))))

(deftest defsum-errors-test
  (testing "members must be distinct parsed types"
    (is (re-find #"members must be distinct" (str (compile-error '(pt/defsum Bad [Point Point])))))
    (is (re-find #"members must be distinct" (str (compile-error '(pt/defsum Bad [Point String])))))))

;;; pt/draft

(deftest draft-test
  (testing "a literal map names every field"
    (is (= (->PointDraft 1 nil) (pt/draft Point {:x 1, :y nil}))))
  (testing "a literal map that leaves a field out does not compile"
    (is (re-find #"Point needs every field; missing \[:y\]" (str (compile-error '(pt/draft Point {:x 1}))))))
  (testing "a literal map with a key that is not a field does not compile"
    (is (re-find #"Point has no fields \[:z\]; its fields are \[:x :y\]"
                 (str (compile-error '(pt/draft Point {:x 1, :z 2}))))))
  (testing "a map built at run time: a key that is not a field is a 400"
    (let [m {:x 1, :z 2}]
      (is (= {:status-code 400, :unknown-fields [:z], :fields [:x :y]}
             (try (pt/draft Point m) (catch clojure.lang.ExceptionInfo e (ex-data e)))))
      (is (= (->PointDraft 1 2) (pt/draft Point (dissoc (assoc m :y 2) :z))))))
  (testing "a type without a draft does not compile"
    (is (re-find #"not a type defined with defparsed" (str (compile-error '(pt/draft String {})))))))

;;; Keys that are not fields

(deftest unknown-keys-test
  (let [d (->PointDraft 1 2)
        p (pt/parse (->PointDraft 2 2))]
    (doseq [[label x] [["a draft" d] ["a parsed value" p]]]
      (testing label
        (doseq [[how thunk] [["keyword" #(:z x)]
                             ["get" #(get x :z)]
                             ["invoke" #(x :z)]
                             ["destructuring" #(let [{:keys [z]} x] z)]]]
          (is (= 500 (:status-code (try (thunk) (catch clojure.lang.ExceptionInfo e (ex-data e))))) how))
        (is (= ::none (get x :z ::none)))
        (is (false? (contains? x :z)))
        (is (nil? (find x :z)))
        (is (= [:x :y] (keys x)))))))

;;; REPL reloads

(defn- eval-here [form]
  (binding [*ns* (the-ns 'parsed-types.defparsed-test)] (eval form)))

(deftest re-evaluating-a-child-type-test
  (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
  (eval-here '(pt/defparsed Parent [kids] (children kids Kid)))
  (let [old-kid ((resolve 'parsed-types.defparsed-test/->KidDraft) 1)
        parent  ((resolve 'parsed-types.defparsed-test/->ParentDraft) [old-kid])]
    (is (some? (pt/parse parent)))
    (testing "after the child type is re-evaluated, the parent no longer accepts the old child, remembered or not"
      (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
      (is (= 400 (:status-code (try (pt/parse parent) (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
    (testing "a draft of the old class does not stop the new class from parsing"
      (is (some? (pt/check old-kid nil)))
      (is (some? (pt/parse ((resolve 'parsed-types.defparsed-test/->KidDraft) 2)))))))

;;; Concurrent first use

(pt/defparsed-type Raced [a])

(def ^:private raced-declaration (atom nil))

(pt/defdraft-type RacedDraft [a]
  @raced-declaration)

(defn- raced-a-int [a] (int? a))

(deftest concurrent-first-use-test
  (testing "threads that plan a declaration at once all keep their remembered passes"
    (dotimes [_ 20]
      (reset! raced-declaration {:parses-to Raced
                                 :checks    [{:name ::a-int, :doc "a is an integer.", :kind :pure, :reads [:a]
                                              :f raced-a-int}]})
      (let [drafts (vec (repeatedly 16 #(->RacedDraft 1)))
            start  (promise)
            fs     (mapv (fn [d] (future @start (pt/check d nil))) drafts)]
        (deliver start true)
        (run! deref fs)
        (is (every? #(= #{::a-int} (pt/passed-checks %)) drafts))))))

;;; Field names

(pt/defparsed Awkward [count map set str seq into contains? or when keys]
  (pure ::count-int "count is an integer." [count] (int? count)))

(deftest fields-named-like-core-functions-test
  (testing "a field may be named like any clojure.core function or macro"
    (let [d (->AwkwardDraft 1 2 3 4 5 6 7 8 9 10)
          p (pt/parse d)]
      (is (= {:count 1, :map 2, :set 3, :str 4, :seq 5, :into 6, :contains? 7, :or 8, :when 9, :keys 10} (into {} p)))
      (is (= 10 (count p) (count d)))
      (is (contains? p :map))
      (is (= [:count 1] (first (seq p))))
      (is (= (hash {:count 1, :map 2, :set 3, :str 4, :seq 5, :into 6, :contains? 7, :or 8, :when 9, :keys 10})
             (hash p)))
      (is (string? (str p)))
      (is (re-find #"#parsed_types.defparsed_test.Awkward\{" (pr-str p)))
      (is (= 2 (:map (assoc d :map 2))))
      (is (= 9 (let [{:keys [when]} p] when)))
      (is (= [:count :map :set :str :seq :into :contains? :or :when :keys] (keys p))))))

(deftest special-form-field-names-test
  (doseq [field '[if do def fn* let* recur throw & new quote]]
    (testing (str field)
      (is (re-find #"special form|simple symbols"
                   (str (compile-error (list 'pt/defparsed 'Special [field]))))))))

(deftest reserved-check-names-test
  (testing "parsed-types.child names belong to the child-field checks"
    (is (re-find #"reserved for child-field checks"
                 (str (compile-error '(pt/defparsed R [kids]
                                        (children kids Point)
                                        (pure :parsed-types.child/kids "d" [kids] false))))))))

(def ^:private order (atom []))

(deftest draft-evaluates-a-literal-map-in-the-order-written-test
  (reset! order [])
  (pt/draft Point {:y (do (swap! order conj :y) 2), :x (do (swap! order conj :x) 1)})
  (is (= [:y :x] @order)))

;;; Equality, and the map interfaces

(pt/defparsed Money [amount count])
(pt/defparsed OtherMoney [amount count])

(deftest equality-test
  (testing "like a record: fields compare with =, so numbers of different classes but equal value are equal"
    (is (= (->Money 1.0M (int 2)) (->Money 1.00M 2)))
    (is (= (hash (->Money 1.0M (int 2))) (hash (->Money 1.00M 2))))
    (is (= (->Money [1N] 2) (->Money [1] 2))))
  (testing "a value equals only a value of its own type"
    (is (not= (->Money 1M 2) (->Money 1M 3)) "every field is compared, not only the first")
    (is (not= (->Money 1M 2) (->OtherMoney 1M 2)))
    (is (not= (->Money 1M 2) (->MoneyDraft 1M 2)))
    (is (not= (->Money 1M 2) {:amount 1M, :count 2}))
    (is (not= {:amount 1M, :count 2} (->Money 1M 2)))
    (is (not (.equals (->Money 1M 2) {:amount 1M, :count 2})))))

(deftest java-map-interface-test
  (let [m ^java.util.Map (->Money 1M 2)]
    (is (= 1M (.get m :amount)))
    (is (nil? (.get m :nope)) "java.util.Map.get returns null for a missing key, as the interface says")
    (is (.containsKey m :count))
    (is (.containsValue m 2))
    (is (not (.isEmpty m)))
    (is (= 2 (.size m)))
    (is (= #{:amount :count} (set (.keySet m))))
    (is (= #{[:amount 1M] [:count 2]} (set (map (fn [^java.util.Map$Entry e] [(.getKey e) (.getValue e)]) (.entrySet m)))))
    (is (thrown? UnsupportedOperationException (.put m :amount 2M)))
    (is (= {} (empty m)))
    (is (= [[:amount 1M] [:count 2]] (reduce-kv (fn [acc k v] (conj acc [k v])) [] m)) "kvreduce in field order")))

(deftest conj-test
  (let [d (->MoneyDraft 1M 2)]
    (is (= 5M (:amount (conj d [:amount 5M]))))
    (is (= 5M (:amount (conj d (clojure.lang.MapEntry/create :amount 5M)))))
    (is (= 5M (:amount (conj d (java.util.AbstractMap$SimpleEntry. :amount 5M)))) "a Java map entry, which is no vector")
    (is (= 5M (:amount (conj d {:amount 5M}))))
    (is (= 5M (:amount (conj d (java.util.HashMap. {:amount 5M})))))
    (is (identical? d (conj d nil)))
    (is (thrown? IllegalArgumentException (conj d [:amount 5M :extra])))))

(deftest a-field-cannot-shadow-the-declaration-test
  (testing "a field named like the generated declaration var does not replace the checks"
    (eval-here '(pt/defparsed Shadow [ShadowDraft-declaration]
                  (pure ::never "Never passes." [ShadowDraft-declaration] false)))
    (let [d ((resolve 'parsed-types.defparsed-test/->ShadowDraft) {:parses-to (resolve 'parsed-types.defparsed-test/Shadow)
                                                                  :checks []})]
      (is (= 400 (:status-code (try (pt/parse d) (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

(deftest field-name-limits-test
  (doseq [[fields re] [['[a.b] #"cannot contain a dot"]
                       ['[a-b a_b] #"compile to the same Java name"]
                       ['[__meta] #"deftype reserves these field names"]]]
    (is (re-find re (str (compile-error (list 'pt/defparsed 'Limits fields)))))))
