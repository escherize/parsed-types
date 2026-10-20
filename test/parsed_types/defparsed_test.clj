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
            #"Write \(defparsed Name \[fields\]"]
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
           ["an options map on a pure check"
            '(pt/defparsed A [a] (pure ::c "d" {:status 422} [a] true))
            #"checks take no options"]
           ["an options map on an external check"
            '(pt/defparsed A [a] (external ::c "d" {} [a] true))
            #"checks take no options"]
           ["an unknown clause"
            '(pt/defparsed A [a] (check ::c "d" [a] true))
            #"A clause is \(pure"]]]
    (testing label
      (is (re-find re (str (compile-error form))))))
  (testing "a compile error is a ::pt/bad-declaration"
    (is (= ::pt/bad-declaration
           (try (binding [*ns* (the-ns 'parsed-types.defparsed-test)]
                  (eval '(pt/defparsed A [a] (pure ::c "d" {} [a] true))))
                nil
                (catch Throwable e (:type (ex-data (or (ex-cause e) e)))))))))

(pt/defparsed Point [x y]
  (pure ::x-int "x is an integer." [x] (int? x))
  (pure ::y-int "y is an integer." [y] (int? y))
  (pure ::on-diagonal "x equals y." [x y] (= x y))
  (external ::known "The point is known." [] [row ::row] (some? row))
  (fetch ::row [x] (fn [tuples] (into {} (map (fn [t] [t :row])) tuples))))

(deftest declaration-test
  (testing "defparsed writes a plain declaration: the data you would write by hand"
    (let [decl (pt/declaration (->PointDraft 1 1))]
      (is (= Point (:parses-to decl)))
      (is (= [{:name ::x-int, :doc "x is an integer.", :kind :pure, :reads [:x]}
              {:name ::y-int, :doc "y is an integer.", :kind :pure, :reads [:y]}
              {:name ::on-diagonal, :doc "x equals y.", :kind :pure, :reads [:x :y]}
              {:name ::known, :doc "The point is known.", :kind :external, :reads [], :fetches [::row]}]
             (mapv #(dissoc % :f) (:checks decl))))
      (is (= [{:name ::row, :reads [:x]}] (mapv #(dissoc % :f) (:fetches decl))))))
  (testing "and parses with it"
    (is (= {:x 2, :y 2} (into {} (pt/parse (->PointDraft 2 2)))))
    (is (= #{:pure} (:failed-kinds (try (pt/parse (->PointDraft 1 :y)) (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(pt/defparsed Other [x z])

(pt/defsum Shape [Point Other])

(deftest only-defparsed-makes-types-test
  (testing "the macros that defined a parsed type or a draft type alone are private, so calling them does not compile"
    (is (re-find #"defdraft-type is not public" (str (compile-error '(pt/defdraft-type HandDraft [a] {})))))
    (is (re-find #"defparsed-type is not public" (str (compile-error '(pt/defparsed-type Hand [a])))))))

(defn- eval-here [form]
  (binding [*ns* (the-ns 'parsed-types.defparsed-test)] (eval form)))

(defn- load-error
  "The ex-data and message of the error loading `form` in this namespace, or nil when it loads."
  [form]
  (try (binding [*ns* (the-ns 'parsed-types.defparsed-test)] (eval form)) nil
       (catch Throwable e
         (let [e (if (ex-data e) e (ex-cause e))] [(ex-data e) (ex-message e)]))))

(deftest declaration-errors-at-load-test
  (testing "what defparsed cannot see when it expands throws ::bad-declaration when the form loads, before any parse"
    (doseq [[label form re]
            [["a fetch that is not a fn"
              '(pt/defparsed Loaded [a] (external ::c "d" [] [r ::f] (some? r)) (fetch ::f [a] 42))
              #"the fetch :parsed-types.defparsed-test/f is 42, not a fn"]
             ["a child type that is not a parsed type"
              '(pt/defparsed Loaded [a] (child a String))
              #"the child field :a holds java.lang.String, which is not a type defined with defparsed"]]]
      (testing label
        (let [[data message] (load-error form)]
          (is (= ::pt/bad-declaration (:type data)))
          (is (re-find re (str message)))))))
  (testing "a pure check that closes over a local: defparsed inside a let, which compiles once the type exists"
    (eval-here '(pt/defparsed Guarded [a]))
    (let [[data message] (load-error '(let [x 1] (pt/defparsed Guarded [a] (pure ::c "d" [a] (= a x)))))]
      (is (= ::pt/bad-declaration (:type data)))
      (is (re-find #"the pure check :parsed-types.defparsed-test/c closes over locals \[x\]" (str message))))))

(deftest match-compile-errors-test
  (doseq [[label form re]
          [["a member left out"
            '(pt/match Shape x Point p 1)
            #"does not handle \[Other\]"]
           ["a type that is not a member"
            '(pt/match Shape x Point p 1 Other o 2 String s 3)
            #"not members: \[String\]"]
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
    (is (re-find #"members are distinct types" (str (compile-error '(pt/defsum Bad [Point Point])))))
    (is (re-find #"members are distinct types" (str (compile-error '(pt/defsum Bad [Point String])))))))

;;; pt/draft

(deftest draft-test
  (testing "a literal map names every field"
    (is (= (->PointDraft 1 nil) (pt/draft Point {:x 1, :y nil}))))
  (testing "a literal map that leaves a field out does not compile"
    (is (re-find #"Point needs every field; missing \[:y\]" (str (compile-error '(pt/draft Point {:x 1}))))))
  (testing "a literal map with a key that is not a field does not compile"
    (is (re-find #"Point has no fields \[:z\]\. A literal map names only fields; its fields are \[:x :y\]"
                 (str (compile-error '(pt/draft Point {:x 1, :z 2}))))))
  (testing "a map built at run time: a key that is not a field is bad input"
    (let [m {:x 1, :z 2}]
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/draft-unknown-keys, :unknown-fields [:z], :fields [:x :y]}
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
          (is (= ::pt/no-field (:type (try (thunk) (catch clojure.lang.ExceptionInfo e (ex-data e))))) how))
        (is (= ::none (get x :z ::none)))
        (is (false? (contains? x :z)))
        (is (nil? (find x :z)))
        (is (= [:x :y] (keys x)))))))

;;; REPL reloads

(defn- resolve-here [sym] @(ns-resolve 'parsed-types.defparsed-test sym))

(deftest re-evaluating-a-child-type-test
  (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
  (eval-here '(pt/defparsed Parent [kids] (children kids Kid)))
  (eval-here '(defn- kid-sink [x] (pt/parsed Kid x)))
  (let [old-kid ((resolve-here '->KidDraft) 1)
        parent  ((resolve-here '->ParentDraft) [old-kid])
        sink    (resolve-here 'kid-sink)]
    (is (some? (pt/parse parent)))
    (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
    (let [new-kid ((resolve-here '->KidDraft) 2)]
      (is (not= (class old-kid) (class new-kid)) "the reload made a new class")
      (testing "a parent defined before the reload accepts drafts of the new child class, like a var"
        (let [p (pt/parse ((resolve-here '->ParentDraft) [new-kid]))]
          (is (= [{:n 2}] (map #(into {} %) (:kids p))))
          (is (every? #(pt/parsed? (class (pt/parse new-kid)) %) (:kids p)))))
      (testing "a draft of the old class still parses, with the plan it was defined with"
        (is (some? (pt/parse parent)))
        (is (some? (pt/parse old-kid))))
      (testing "a sink compiled before the reload accepts values of the new class"
        (is (= {:n 2} (into {} (sink (pt/parse new-kid)))))
        (is (= ::pt/not-parsed (:type (try (sink new-kid) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))))))))

(deftest re-evaluating-a-member-of-a-sum-test
  (eval-here '(pt/defparsed Member [n]))
  (eval-here '(pt/defparsed NonMember [n]))
  (eval-here '(pt/defsum Members [Member NonMember]))
  (eval-here '(pt/defparsed Holder [m] (child m Members)))
  (eval-here '(defn- which-member [x] (pt/match Members x Member m [:member (:n m)] NonMember o [:non-member (:n o)])))
  (eval-here '(pt/defparsed Member [n]))
  (let [member (pt/parse ((resolve-here '->MemberDraft) 1))]
    (testing "a holder defined against the sum before the reload accepts a draft of the new member class"
      (is (= {:n 1} (into {} (:m (pt/parse ((resolve-here '->HolderDraft) ((resolve-here '->MemberDraft) 1))))))))
    (testing "the sum, and a match compiled before the reload, accept the new class"
      (is (pt/parsed? (resolve-here 'Members) member))
      (is (= [:member 1] ((resolve-here 'which-member) member))))
    (testing "a match compiled after the reload, against the old defsum, compiles"
      (is (nil? (compile-error '(fn [x] (pt/match Members x Member m (:n m) NonMember o (:n o)))))))))

(deftest a-hand-written-class-with-the-same-name-is-refused-test
  (eval-here '(pt/defparsed Genuine [n]))
  (let [genuine (ns-resolve 'parsed-types.defparsed-test 'Genuine)
        real    (pt/parse ((resolve-here '->GenuineDraft) 1))]
    (eval-here '(deftype Genuine [n] parsed_types.core.ParsedValue (issuedWith [_ _] true)))
    (let [fake (eval-here '(Genuine. 1))]
      (is (= (.getName ^Class genuine) (.getName (class fake))) "same fully qualified name")
      (testing "parsed? asks the registry for the class itself, so the fake is refused"
        (is (false? (pt/parsed? genuine fake)))
        (is (= ::pt/not-parsed (:type (try (pt/parsed genuine fake) nil
                                           (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
      (testing "while membership is still by name: the real value counts as the type"
        (is (pt/parsed? genuine real))))))

(defn- eval-in
  "Evaluate `form` in the namespace `ns-sym`, created with clojure.core referred and parsed-types.core as pt."
  [ns-sym form]
  (binding [*ns* (create-ns ns-sym)]
    (refer-clojure)
    (require '[parsed-types.core :as pt])
    (eval form)))

(deftest a-class-name-belongs-to-one-namespace-test
  (eval-in 'pt-owner.my-app.bank '(pt/defparsed Debit [amount] (pure ::pos "The amount is positive." [amount] (pos? amount))))
  (let [real-type (eval-in 'pt-owner.my-app.bank 'Debit)
        sink      (eval-in 'pt-owner.my-app.bank '(fn [d] (:amount (pt/parsed Debit d))))]
    (testing "a namespace whose name compiles to the same Java package cannot define a type of that name"
      (let [e (try (eval-in 'pt-owner.my_app.bank '(pt/defparsed Debit [amount])) nil
                   (catch Throwable t (if (ex-data t) t (ex-cause t))))]
        (is (= {:type  ::pt/bad-declaration, :rule :parsed-types.rule/class-name-taken
                :class "pt_owner.my_app.bank.Debit", :owner 'pt-owner.my-app.bank, :ns 'pt-owner.my_app.bank}
               (select-keys (ex-data e) [:type :rule :class :owner :ns])))
        (is (re-find #"already defined by a defparsed in the namespace pt-owner.my-app.bank" (ex-message e)))
        (is (re-find #"Rename one of the namespaces" (ex-message e)))))
    (testing "so its drafts parse to nothing, and the sink accepts nothing from it"
      (is (= ::pt/bad-input (:type (try (eval-in 'pt-owner.my_app.bank '(pt/parse (->DebitDraft -1000))) nil
                                        (catch Throwable t (ex-data (if (ex-data t) t (ex-cause t)))))))))
    (testing "the owning namespace may redefine it, as a REPL reload does"
      (eval-in 'pt-owner.my-app.bank '(pt/defparsed Debit [amount] (pure ::pos "The amount is positive." [amount] (pos? amount))))
      (is (= 5 (sink (eval-in 'pt-owner.my-app.bank '(pt/parse (->DebitDraft 5))))))
      (is (= (.getName ^Class real-type) (.getName ^Class (eval-in 'pt-owner.my-app.bank 'Debit)))))))

(deftest a-match-compiled-before-a-member-joined-throws-test
  (eval-here '(pt/defparsed MA [x]))
  (eval-here '(pt/defparsed MB [x]))
  (eval-here '(pt/defparsed MC [x]))
  (eval-here '(pt/defsum Grows [MA MB]))
  (eval-here '(defn- grows-match [v] (pt/match Grows v MA a [:a a] MB b [:b b])))
  (eval-here '(pt/defsum Grows [MA MB MC]))
  (let [e (try ((resolve-here 'grows-match) (pt/parse ((resolve-here '->MCDraft) 1))) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type ::pt/not-parsed, :rule :parsed-types.rule/match-compiled-before-member, :handled '[MA MB]}
           (select-keys (ex-data e) [:type :rule :handled])))
    (is (re-find #"compiled before MC joined the sum.*Re-evaluate the match" (ex-message e))))
  (testing "values that are not parsed still get the sink's own error"
    (is (= :parsed-types.rule/got-draft
           (:rule (try ((resolve-here 'grows-match) ((resolve-here '->MCDraft) 1)) nil
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest is?-answers-the-type-without-the-proof-test
  (eval-here '(pt/defparsed IsLine [amount]))
  (eval-here '(pt/defparsed IsOther [amount]))
  (eval-here '(pt/defsum IsSum [IsLine IsOther]))
  ;; like a JournalEntry whose ::balanced check filters its lines by type
  (eval-here '(pt/defparsed IsEntry [lines]
                (children lines IsSum)
                (pure ::unproven "Inside the check, no line is parsed yet." [lines]
                      (not-any? #(pt/parsed? IsSum %) lines))
                (pure ::balanced "The IsLine amounts add up to the IsOther amounts." [lines]
                      (and (seq (filter #(pt/is? IsLine %) lines))
                           (= (reduce + (map :amount (filter #(pt/is? IsLine %) lines)))
                              (reduce + (map :amount (filter #(pt/is? IsOther %) lines))))))))
  (let [line-draft #((resolve-here '->IsLineDraft) 1)
        entry      #(pt/parse ((resolve-here '->IsEntryDraft) [(line-draft) ((resolve-here '->IsOtherDraft) 1)]))
        is-line?   (eval-here '(fn [x] (pt/is? IsLine x)))]
    (testing "inside a check, on a child view that pt/parsed? refuses"
      (is (some? (entry))))
    (testing "on parsed values and drafts, of the type and of a sum"
      (let [p (pt/parse (line-draft))]
        (is (true? (is-line? p)))
        (is (true? (is-line? (line-draft))))
        (is (true? (pt/is? (resolve-here 'IsSum) p)))))
    (testing "false for another type, a plain map, nil, and a value built by hand without the proof is still a value of
              the type: is? is not proof"
      (is (false? (is-line? (pt/parse ((resolve-here '->IsOtherDraft) 1)))))
      (is (false? (is-line? {:amount 1})))
      (is (false? (is-line? nil)))
      (is (true? (is-line? (eval-here '(IsLine. 1 nil))))))
    (testing "after IsLine is re-evaluated, the check and code compiled before it still answer true"
      (eval-here '(pt/defparsed IsLine [amount]))
      (is (some? (entry)) "the entry's ::balanced, compiled against the old IsLine, passes on the new one")
      (is (true? (is-line? (line-draft))))
      (is (false? (eval-here '(instance? (first (:members IsSum)) (pt/parse (->IsLineDraft 1)))))
          "instance? against the old class, which is what is? replaces, says false"))
    (testing "a class written by hand with the type's name does not count"
      (let [fake (eval-here '(do (deftype IsLine [amount] parsed_types.core.ParsedValue (issuedWith [_ _] true))
                                 (IsLine. 1)))]
        (is (false? (is-line? fake)))))
    (testing "a type argument that is not a parsed type or sum throws"
      (is (= :parsed-types.rule/not-a-parsed-type
             (try (pt/is? :x 1) (catch clojure.lang.ExceptionInfo e (:rule (ex-data e)))))))))

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
    (is (= (Money. 1.0M (int 2) nil) (Money. 1.00M 2 nil)))
    (is (= (hash (Money. 1.0M (int 2) nil)) (hash (Money. 1.00M 2 nil))))
    (is (= (Money. [1N] 2 nil) (Money. [1] 2 nil))))
  (testing "a value equals only a value of its own type"
    (is (not= (Money. 1M 2 nil) (Money. 1M 3 nil)) "every field is compared, not only the first")
    (is (not= (Money. 1M 2 nil) (OtherMoney. 1M 2 nil)))
    (is (not= (Money. 1M 2 nil) (->MoneyDraft 1M 2)))
    (is (not= (Money. 1M 2 nil) {:amount 1M, :count 2}))
    (is (not= {:amount 1M, :count 2} (Money. 1M 2 nil)))
    (is (not (.equals (Money. 1M 2 nil) {:amount 1M, :count 2})))))

(deftest java-map-interface-test
  (let [m ^java.util.Map (Money. 1M 2 nil)]
    (is (= 1M (.get m :amount)))
    (is (nil? (.get m :nope)) "java.util.Map.get returns null for a missing key, as the interface says")
    (is (.containsKey m :count))
    (is (.containsValue m 2))
    (is (not (.isEmpty m)))
    (is (= 2 (.size m)))
    (is (= #{:amount :count} (set (.keySet m))))
    (is (= #{[:amount 1M] [:count 2]} (set (map (fn [^java.util.Map$Entry e] [(.getKey e) (.getValue e)]) (.entrySet m)))))
    (doseq [[op thunk] [["put" #(.put m :amount 2M)] ["putAll" #(.putAll m {:amount 2M})]
                        ["remove" #(.remove m :amount)] ["clear" #(.clear m)]]]
      (is (= {:type ::pt/cannot-change, :op op}
             (select-keys (try (thunk) nil (catch clojure.lang.ExceptionInfo e (ex-data e))) [:type :op]))
          "a java.util.Map mutator throws ex-info, a RuntimeException like UnsupportedOperationException"))
    (is (= ::pt/cannot-change (try (empty m) (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
    (is (= [[:amount 1M] [:count 2]] (reduce-kv (fn [acc k v] (conj acc [k v])) [] m)) "kvreduce in field order")))

(deftest conj-test
  (let [d (->MoneyDraft 1M 2)]
    (is (= 5M (:amount (conj d [:amount 5M]))))
    (is (= 5M (:amount (conj d (clojure.lang.MapEntry/create :amount 5M)))))
    (is (= 5M (:amount (conj d (java.util.AbstractMap$SimpleEntry. :amount 5M)))) "a Java map entry, which is no vector")
    (is (= 5M (:amount (conj d {:amount 5M}))))
    (is (= 5M (:amount (conj d (java.util.HashMap. {:amount 5M})))))
    (is (identical? d (conj d nil)))
    (is (= :parsed-types.rule/conj-not-a-pair
           (try (conj d [:amount 5M :extra]) (catch clojure.lang.ExceptionInfo e (:rule (ex-data e))))))))

(deftest a-field-cannot-replace-the-checks-test
  (testing "a field named like a declaration, holding one, is only a field: the checks stay those defparsed wrote"
    (eval-here '(pt/defparsed Shadow [ShadowDraft-declaration]
                  (pure ::never "Never passes." [ShadowDraft-declaration] false)))
    (let [d ((resolve 'parsed-types.defparsed-test/->ShadowDraft) {:parses-to (resolve 'parsed-types.defparsed-test/Shadow)
                                                                  :checks []})]
      (is (= #{:pure} (:failed-kinds (try (pt/parse d) (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

(deftest field-name-limits-test
  (doseq [[fields re] [['[a.b] #"cannot contain a dot"]
                       ['[a-b a_b] #"compile to the same Java name"]
                       ['[__meta] #"deftype reserves these field names"]
                       ['[parse-proof] #"reserves the field name parse-proof"]
                       ['[parse_proof] #"reserves the field name parse-proof"]]]
    (is (re-find re (str (compile-error (list 'pt/defparsed 'Limits fields)))))))
