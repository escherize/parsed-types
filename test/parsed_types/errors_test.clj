(ns parsed-types.errors-test
  "Every error the library throws, one example per rule. Each message names the concrete values and the fix, and the
  ex-data names the rule. The rules listed here must be exactly the rules core.clj can throw."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

;; public: only the quoted forms below use it
(defn rows [tuples] (into {} (map (fn [t] [t {:ok true}])) tuples))

(defn- forgetful-rows [_tuples] {})

(defn- vector-rows [_tuples] [:not-a-map])

(pt/defparsed Debit [user-id account-id amount]
  (pure ::amount-positive "The amount is positive." [amount] (and (number? amount) (pos? amount)))
  (pure ::account-id-int "The account id is an integer." [account-id] (int? account-id)))

(pt/defparsed Credit [amount])

(pt/defsum Txn [Debit Credit])

(pt/defparsed Lossy [id]
  (external ::row-ok "The row is ok." [] [row ::row] (some? row))
  (fetch ::row [id] forgetful-rows))

(pt/defparsed Odd [id]
  (external ::row-ok "The row is ok." [] [row ::row] (some? row))
  (fetch ::row [id] vector-rows))

(pt/defparsed Chain [next]
  (child next Chain))

(pt/defparsed Wide [parts]
  (children parts Wide))

(pt/defparsed Seven [a b c d e f g]
  (pure ::a "a is an integer." [a] (int? a))
  (pure ::b "b is an integer." [b] (int? b))
  (pure ::c "c is an integer." [c] (int? c))
  (pure ::d "d is an integer." [d] (int? d))
  (pure ::e "e is an integer." [e] (int? e))
  (pure ::f "f is an integer." [f] (int? f))
  (pure ::g "g is an integer." [g] (int? g)))

(defn- error
  "What `thunk` threw: the first ex-info with a `:type`, through compiler wrapping, as `{:type :rule :message}`."
  [thunk]
  (try (thunk) nil
       (catch Throwable t
         (when-let [e (loop [e t] (cond (nil? e) nil, (:type (ex-data e)) e, :else (recur (ex-cause e))))]
           {:type (:type (ex-data e)), :rule (:rule (ex-data e)), :message (ex-message e)}))))

(defn- compiling [form]
  #(binding [*ns* (the-ns 'parsed-types.errors-test)] (eval form)))

(defn- in-ns-named
  "A thunk that evaluates `form` in the namespace `ns-sym`, created with clojure.core and parsed-types.core as pt."
  [ns-sym form]
  #(binding [*ns* (create-ns ns-sym)]
     (refer-clojure)
     (require '[parsed-types.core :as pt])
     (eval form)))

(def ^:private real (pt/parse (->DebitDraft 1 2 30M)))

(def ^:private draft (->DebitDraft 1 2 30M))

(def ^:private examples
  "`[rule thunk substring ...]`: the thunk throws the rule, and its message holds every substring."
  [;; changing values
   [:parsed-value-is-final #(assoc real :amount 5) "assoc :amount on a Debit" "(pt/parse (assoc (pt/draft-of x) :amount v))"]
   [:parsed-no-metadata #(with-meta real {}) "with-meta on a Debit" "{:value x, :meta m}"]
   [:parsed-no-empty #(empty real) "Debit has fixed fields" "(into {} x)"]
   [:draft-fields-are-fixed #(dissoc draft :amount) "dissoc :amount on a DebitDraft" "(assoc d :amount nil)"]
   [:draft-no-metadata #(with-meta draft {}) "with-meta on a DebitDraft" "{:value x, :meta m}"]
   [:draft-no-empty #(empty draft) "DebitDraft has fixed fields" "(into {} x)"]
   [:conj-not-a-pair #(conj draft [:amount 1 2]) "conj of [:amount 1 2] on a DebitDraft" "has 3 elements"
    "(conj x [k v]), or (assoc x k v)"]
   [:no-field-not-a-library-value #(pt/no-field! 5 :k "read") "pt/no-field! got 5" "(get m k default)"]
   [:no-such-field-read #(:amont real) "Debit has no field :amont" "Did you mean :amount?"
    "[:user-id :account-id :amount]" "(:amount x)"]
   [:no-such-field-assoc #(assoc draft :acount-id 1) "DebitDraft has no field :acount-id" "Did you mean :account-id?"
    "(assoc d :account-id v)"]
   ;; field names
   [:fields-not-symbols (compiling '(pt/defparsed E1 [:a])) "the fields [:a]" "[user-id amount]"]
   [:field-named-twice (compiling '(pt/defparsed E1 [a a])) "named twice: [a]" "Name each field once" "In: (pt/defparsed"]
   [:field-slash (compiling '(pt/defparsed E1 [/])) "cannot be named /" "as in slash"]
   [:field-special-form (compiling '(pt/defparsed E1 [if])) "[if]" "if-value"]
   [:field-with-dot (compiling '(pt/defparsed E1 [a.b])) "[a.b]" "a-b"]
   [:fields-same-java-name (compiling '(pt/defparsed E1 [a-b a_b])) "[a-b a_b]" "Rename one of them"]
   [:field-reserved-by-deftype (compiling '(pt/defparsed E1 [__meta])) "[__meta]" "as in meta"]
   [:field-reserved-for-proof (compiling '(pt/defparsed E1 [parse-proof])) "[parse-proof]" "as in proof"]
   ;; sums and match
   [:bad-sum-members (compiling '(pt/defsum Bad [Debit Debit])) "[Debit Debit]" "(defsum Bad [Debit Credit])"]
   [:match-not-a-sum (compiling '(fn [x] (pt/match Debit x Debit d 1))) "Debit is not a sum" "(pt/defsum Transaction [Debit Credit])"]
   [:match-clause-shape (compiling '(fn [x] (pt/match Txn x Debit d))) "2 clause forms" "Debit d (f d)"]
   [:match-not-a-type (compiling '(fn [x] (pt/match Txn x nope d 1 Credit c 2))) "nope is not a type"
    "[Debit Credit]"]
   [:match-missing-member (compiling '(fn [x] (pt/match Txn x Debit d 1))) "does not handle [Credit]"
    "Credit x (f x)"]
   [:match-not-a-member (compiling '(fn [x] (pt/match Txn x Debit d 1 Credit c 2 String s 3))) "not members: [String]"
    "add the types to the defsum"]
   [:match-member-twice (compiling '(fn [x] (pt/match Txn x Debit d 1 Credit c 2 Debit e 3)))
    "handles a type twice: [Debit]" "Keep one clause per type"]
   ;; defparsed clauses
   [:name-not-symbol (compiling '(pt/defparsed "E1" [a])) "the name \"E1\"" "(defparsed Name [fields]"]
   [:unknown-clause (compiling '(pt/defparsed E1 [a] (check ::c "d" [a] true))) "check does not start a clause"
    "(pure ...)"]
   [:check-options (compiling '(pt/defparsed E1 [a] (pure ::c "d" {:x 1} [a] true)))
    "the check :parsed-types.errors-test/c has options {:x 1}" "Remove {:x 1}"]
   [:check-no-body-after-fetches (compiling '(pt/defparsed E1 [a] (external ::c "d" [] [r ::f]) (fetch ::f [a] rows)))
    "ends after its fetch bindings [r :parsed-types.errors-test/f]" "(some? r)"]
   [:check-name-not-qualified (compiling '(pt/defparsed E1 [a] (pure :c "d" [a] true))) "the check :c" "::c"]
   [:check-no-doc (compiling '(pt/defparsed E1 [a] (pure ::c [a] true))) "the check :parsed-types.errors-test/c"
    "needs a doc string" "\"The amount is positive.\""]
   [:pure-check-fetches (compiling '(pt/defparsed E1 [a] (pure ::c "d" [a] [r ::f] true) (fetch ::f [a] rows)))
    "binds fetches [r :parsed-types.errors-test/f]" "Make it (external ...)"]
   [:check-no-field-vector (compiling '(pt/defparsed E1 [a] (pure ::c "d" a true))) "has a, not a vector of the fields it reads" "like [a]"]
   [:check-reads-unknown-field (compiling '(pt/defparsed E1 [amount] (pure ::c "d" [amont] true))) "[amont]"
    "Did you mean amount?" "[amount]"]
   [:check-reads-field-twice (compiling '(pt/defparsed E1 [a] (pure ::c "d" [a a] true))) "reads a field twice: [a]"
    "Name each field once"]
   [:check-bad-fetch-binding (compiling '(pt/defparsed E1 [a] (external ::c "d" [a] [x ::nope] true)))
    "binds [x :parsed-types.errors-test/nope]" "[account ::account]"
    "(fetch ::account [account-id] accounts-by-id)"]
   [:check-two-fetches-one-local (compiling '(pt/defparsed E1 [a] (external ::c "d" [] [r ::f r ::g] true)
                                               (fetch ::f [a] rows) (fetch ::g [a] rows)))
    "two fetches to one local: [r]" "Give each fetch its own local"]
   [:fetch-bound-to-field (compiling '(pt/defparsed E1 [a] (external ::c "d" [] [a ::f] true) (fetch ::f [a] rows)))
    "bound to a field's name: [a]" "as in a-row"]
   [:check-no-body (compiling '(pt/defparsed E1 [a] (pure ::c "d" [a]))) "needs a body" "(some? a)"]
   [:check-uses-unnamed-field (compiling '(pt/defparsed E1 [a b] (pure ::c "d" [a] (< a b)))) "does not name: [b]"
    "Add them to the argument vector: [a b]"]
   [:fetch-name-not-qualified (compiling '(pt/defparsed E1 [a] (fetch :f [a] rows))) "the fetch :f" "::f"]
   [:fetch-bad-fields (compiling '(pt/defparsed E1 [account-id] (fetch ::f [acount-id] rows))) "reads [acount-id]"
    "Did you mean account-id?" "[account-id]"]
   [:fetch-no-fn (compiling '(pt/defparsed E1 [a] (fetch ::f [a]))) "has no function" "rows-by-id"]
   [:child-not-a-field (compiling '(pt/defparsed E1 [kids] (children kid Debit))) "the child field kid is not a field of the draft"
    "Did you mean kids?"]
   [:child-declared-twice (compiling '(pt/defparsed E1 [a] (child a Debit) (children a Debit)))
    "declared as a child twice: [a]" "Declare it once"]
   [:name-used-twice (compiling '(pt/defparsed E1 [a] (pure ::c "d" [a] true) (pure ::c "d" [a] true)))
    "used twice: [:parsed-types.errors-test/c]" "Give each its own name"]
   [:reserved-name (compiling '(pt/defparsed E1 [kids] (children kids Debit) (pure :parsed-types.child/kids "d" [kids] true)))
    "[:parsed-types.child/kids]" "as in ::kids"]
   ;; when a defparsed loads
   [:child-type-not-parsed (compiling '(pt/defparsed E2 [a] (child a String))) "the child field :a holds java.lang.String"
    "(child a java.lang.String)" "Name one of those"]
   [:fetch-not-fn (compiling '(pt/defparsed E2 [a] (external ::c "d" [] [r ::f] (some? r)) (fetch ::f [a] 42)))
    "is 42, not a fn" "(fetch :parsed-types.errors-test/f [a] rows-by-id)"]
   [:class-name-taken #(do ((in-ns-named 'errs.my-app '(pt/defparsed Owned [x])))
                           ((in-ns-named 'errs.my_app '(pt/defparsed Owned [x]))))
    "errs.my_app.Owned is already defined by a defparsed in the namespace errs.my-app" "the namespace errs.my_app"
    "Rename one of the namespaces"]
   [:pure-check-closes-over-locals
    ;; a defparsed inside a let compiles once its type exists, as when a REPL re-evaluates it
    #(do ((compiling '(pt/defparsed Guarded [a])))
         ((compiling '(let [x 1] (pt/defparsed Guarded [a] (pure ::c "d" [a] (= a x)))))))
    "the pure check :parsed-types.errors-test/c closes over locals [x]" "make the check external"]
   ;; parsing
   [:not-a-draft #(pt/parse {:amount 1}) "pt/parse got a plain map" "(pt/parse (pt/draft Debit m))"]
   [:mutable-value #(pt/parse (->DebitDraft 1 2 (atom 1))) "the field :amount holds an atom (clojure.lang.Atom)"
    "Deref it, as in @a."]
   [:tree-too-deep #(pt/parse (reduce (fn [c _] (->ChainDraft c)) (->ChainDraft nil) (range 1000)))
    "deeper than 1000 levels" "Parse a shallower tree"]
   [:tree-too-big #(pt/parse (reduce (fn [w _] (->WideDraft [w w])) (->WideDraft []) (range 20)))
    "more than 100000 drafts" "(pt/parse-all parts)"]
   [:fetch-missing-tuples #(pt/parse (->LossyDraft 1)) "returned no entry for [[1]]"
    "(into {} (map (fn [t] [t (lookup t)])) tuples)"]
   [:fetch-not-a-map #(pt/parse (->OddDraft 1)) "returned a PersistentVector, not a map"
    "(into {} (map (fn [t] [t (lookup t)])) tuples)"]
   [:checks-failed #(pt/parse (->DebitDraft 1 :x -5)) "Invalid Debit: 2 checks failed"
    ":parsed-types.errors-test/amount-positive (The amount is positive.)" "Change the draft so they pass"]
   ;; sinks
   [:not-a-parsed-type #(pt/parsed :x real) "pt/parsed got :x as its type" "(pt/parsed Debit x)"]
   [:got-nil #(pt/parsed Debit nil) "got nil" "Write (pt/parse (->DebitDraft ...)) and pass the result"]
   [:got-draft #(pt/parsed Debit draft) "got a DebitDraft" "(pt/parse draft)"]
   [:got-unissued #(pt/parsed Debit (Debit. 1 2 30M nil)) "got a Debit that pt/parse did not issue"
    "Write (pt/parse (->DebitDraft ...)) and pass the result"]
   [:got-plain-map #(pt/parsed Debit {:amount 1}) "got a plain map" "(pt/parse (pt/draft Debit m))"]
   [:got-other-type #(pt/parsed Debit (pt/parse (->CreditDraft 1))) "got a Credit" "(pt/parse (->DebitDraft ...))"]
   [:draft-of-a-draft #(pt/draft-of draft) "got a DebitDraft, which is already a draft" "Use the draft as it is"]
   [:draft-of-unissued #(pt/draft-of (Debit. 1 2 30M nil)) "got a Debit that pt/parse did not issue" "(->DebitDraft ...)"]
   [:match-compiled-before-member
    ;; what a match on Growing compiled when it held only Debit does once Credit joins
    #((var parsed-types.core/match-fell-through!) (pt/sum 'Growing [Debit Credit]) '[Debit]
      (pt/parse (->CreditDraft 1)))
    "got a parsed Credit, but this match was compiled before Credit joined the sum" "Re-evaluate the match"]
   ;; pt/draft
   [:not-a-draft-type #(pt/draft-from-map String {}) "pt/draft-from-map got java.lang.String"
    "(pt/draft-from-map DebitDraft m)" "(pt/draft Debit m)"]
   [:draft-from-non-map #(pt/draft Debit (identity 5)) "pt/draft got a Long to build a DebitDraft" "{:user-id ...}"]
   [:draft-unknown-keys #(pt/draft Debit (identity {:amont 1})) "DebitDraft has no fields [:amont]"
    "Did you mean :amount?" "[:user-id :account-id :amount]"]
   [:draft-not-a-parsed-type (compiling '(pt/draft String {})) "String is not a type defined with defparsed"
    "(pt/draft Debit {...})"]
   [:draft-literal-unknown-keys (compiling '(pt/draft Debit {:user-id 1, :account-id 2, :amont 3}))
    "Debit has no fields [:amont]" "Did you mean :amount?" "[:user-id :account-id :amount]"]
   [:draft-literal-missing-fields (compiling '(pt/draft Debit {:user-id 1, :account-id 2})) "missing [:amount]"
    "as in :amount nil"]])

(deftest every-error-names-what-happened-the-rule-and-the-fix-test
  (doseq [[rule thunk & substrings] examples]
    (testing rule
      (let [{:keys [type message], r :rule} (error thunk)]
        (is (= (keyword "parsed-types.rule" (name rule)) r))
        (is (= "parsed-types.core" (some-> type namespace)))
        (doseq [sub substrings]
          (is (str/includes? (str message) sub) (str "missing " (pr-str sub) " in: " message)))))))

(def ^:private source (slurp (io/file "src/parsed_types/core.clj")))

(deftest every-rule-has-an-example-test
  (testing "the rules core.clj can throw are exactly the rules with an example here"
    (let [in-source (set (map #(keyword (second %)) (re-seq #"::rule/([a-z-]+)" source)))]
      (is (= in-source (set (map first examples)))))))

(deftest every-throw-names-a-rule-test
  (testing "every ex-info form in core.clj puts a :rule in its ex-data"
    (let [forms (binding [*ns* (the-ns 'parsed-types.core), *read-eval* false]
                  (let [r (java.io.PushbackReader. (java.io.StringReader. source))]
                    (doall (take-while #(not= ::eof %) (repeatedly #(read {:eof ::eof} r))))))
          sites (filter #(and (seq? %) (= 'ex-info (first %))) (tree-seq coll? seq forms))]
      (is (seq sites))
      (doseq [site sites]
        (is (some #{:rule} (tree-seq coll? seq (nth site 2))) (pr-str site))))))

(deftest no-other-throws-test
  (testing "core.clj throws only ex-info: no Java exception escapes without a :type and a :rule"
    (is (nil? (re-seq #"\(throw \((?!ex-info)[^\s)]*" source)))))

(deftest the-type-argument-is-checked-test
  (doseq [[label thunk] [["nil" #(pt/parsed nil real)] ["a keyword" #(pt/parsed? :x real)]
                         ["a draft type" #(pt/parsed DebitDraft real)] ["a bare Sum" #(pt/parsed? (pt/sum 'S [Debit]) nil)]]]
    (testing label
      (is (= (if (= "a bare Sum" label) nil :parsed-types.rule/not-a-parsed-type) (:rule (error thunk))))))
  (is (str/includes? (:message (error #(pt/parsed DebitDraft real))) "pass its parsed type, as in (pt/parsed Debit x)")))

(deftest the-java-map-mutators-throw-ex-info-test
  (doseq [[op thunk] [["put" #(.put ^java.util.Map real :amount 1)] ["clear" #(.clear ^java.util.Map draft)]]]
    (is (= ::pt/cannot-change (:type (error thunk))) op))
  (is (str/includes? (:message (error #(.clear ^java.util.Map draft))) "clear on a DebitDraft"))
  (is (str/includes? (:message (error #(.clear ^java.util.Map draft))) "(assoc d :field v)")))

(deftest invalid-names-the-first-five-checks-test
  (is (str/includes? (:message (error #(pt/parse (->SevenDraft :a :b :c :d :e :f :g))))
                     "Invalid Seven: 7 checks failed: :parsed-types.errors-test/a (a is an integer.), ")
      "each failed check with its doc")
  (is (str/includes? (:message (error #(pt/parse (->SevenDraft :a :b :c :d :e :f :g))))
                     "(e is an integer.), and 2 more.")))

(deftest suggestions-are-data-too-test
  (is (= :amount (:suggestion (ex-data (try (:amont real) (catch clojure.lang.ExceptionInfo e e))))))
  (is (= :amount (:suggestion (ex-data (try (pt/draft Debit (identity {:amont 1})) (catch clojure.lang.ExceptionInfo e e))))))
  (testing "no suggestion when nothing is close, or the key is short"
    (is (not (contains? (ex-data (try (:zzzzzz real) (catch clojure.lang.ExceptionInfo e e))) :suggestion)))
    (is (not (contains? (ex-data (try (:x real) (catch clojure.lang.ExceptionInfo e e))) :suggestion)))))
