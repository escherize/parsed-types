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
   [:not-a-library-value #(#'pt/no-field! 5 :k "read") "parsed-types.core/no-field! got 5" "(get m k default)"]
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
   [:sum-name-not-symbol (compiling '(pt/defsum "Bad" [Debit Credit])) "the name \"Bad\" is not a symbol"
    "(defsum Transaction [Debit Credit])"]
   [:match-sum-not-a-symbol (compiling '(fn [x] (pt/match "Txn" x Debit d 1 Credit c 2))) "the sum \"Txn\" is not a symbol"
    "(pt/match Txn x Debit d 1 Credit c 2)"]
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
   [:name-with-dot (compiling '(pt/defparsed C.D [x])) "the name C.D has a dot" "as in CD."]
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
   [:check-uses-unnamed-field (compiling '(pt/defparsed E1 [a b] (pure ::c "d" [a] (< a b))))
    "the check :parsed-types.errors-test/c uses the field `b` without naming it"
    "Add it to the argument vector: [a b]" "If it is a local, rename the local, as in b-value."]
   [:fetch-name-not-qualified (compiling '(pt/defparsed E1 [a] (fetch :f [a] rows))) "the fetch :f" "::f"]
   [:fetch-bad-fields (compiling '(pt/defparsed E1 [account-id] (fetch ::f [acount-id] rows))) "reads [acount-id]"
    "Did you mean account-id?" "[account-id]"]
   [:fetch-reads-child-field (compiling '(pt/defparsed E1 [kids] (children kids Debit) (fetch ::f [kids] rows)))
    "the fetch :parsed-types.errors-test/f reads the child field kids"
    "Read the children in the check itself, as in (external ::check \"doc\" [kids] ...)"]
   [:fetch-no-fn (compiling '(pt/defparsed E1 [a] (fetch ::f [a]))) "has no function" "rows-by-id"]
   [:child-not-a-field (compiling '(pt/defparsed E1 [kids] (children kid Debit))) "the child field kid is not a field of the draft"
    "Did you mean kids?"]
   [:child-declared-twice (compiling '(pt/defparsed E1 [a] (child a Debit) (children a Debit)))
    "declared as a child twice: [a]" "Declare it once"]
   [:name-used-twice (compiling '(pt/defparsed E1 [a] (pure ::c "d" [a] true) (pure ::c "d" [a] true)))
    "used twice: [:parsed-types.errors-test/c]" "Give each its own name"]
   [:pure-check-order (compiling '(pt/defparsed E1 [a b] (pure ::ab "d" [a b] true) (pure ::a "d" [a] true)))
    "the pure check :parsed-types.errors-test/ab reads [a b], and is written before :parsed-types.errors-test/a"
    "Move :parsed-types.errors-test/a before :parsed-types.errors-test/ab."]
   [:reserved-name (compiling '(pt/defparsed E1 [kids] (children kids Debit) (pure :parsed-types.child/kids "d" [kids] true)))
    "[:parsed-types.child/kids]" "as in ::kids"]
   ;; when a defparsed loads
   [:child-type-not-parsed (compiling '(pt/defparsed E2 [a] (child a String))) "the child field :a holds java.lang.String"
    "(child a java.lang.String)" "Name one of those"]
   [:fetch-not-fn (compiling '(pt/defparsed E2 [a] (external ::c "d" [] [r ::f] (some? r)) (fetch ::f [a] 42)))
    "is 42, not a fn" "(fetch :parsed-types.errors-test/f [a] rows-by-id)"]
   [:defparsed-not-top-level (compiling '(let [x 1] (pt/defparsed E3 [a] (pure ::c "d" [a] (= a x)))))
    "(defparsed E3 ...) is inside another form" "Write defparsed at the top level of a namespace" "(def limit 10)"]
   [:type-not-loaded #(do ((compiling '(pt/defparsed E4 [a])))
                          (try ((compiling '(pt/defparsed E4 [a] (pure ::c "d" [a] (no-such-fn a))))) (catch Exception _))
                          (pt/parse ((compiling '(->E4Draft 1)))))
    "pt/parse got a draft of parsed_types.errors_test.E4Draft, but the defparsed E4 in parsed-types.errors-test failed to load"
    "Fix that error and re-evaluate the defparsed E4" "(require 'parsed-types.errors-test :reload)"]
   [:class-name-taken #(do ((in-ns-named 'errs.my-app '(pt/defparsed Owned [x])))
                           ((in-ns-named 'errs.my_app '(pt/defparsed Owned [x]))))
    "errs.my_app.Owned is already defined by a defparsed in the namespace errs.my-app" "the namespace errs.my_app"
    "Rename one of the namespaces"]
   ;; parsing
   [:parse-all-not-a-vector #(pt/parse-all draft) "pt/parse-all got a DebitDraft" "(pt/parse-all [draft])"
    "(pt/parse draft)"]
   [:not-a-draft #(pt/parse {:amount 1}) "pt/parse got a plain map" "(pt/parse (pt/draft Debit {:user-id u, ...}))"]
   [:child-field-metadata #(pt/parse (->WideDraft (with-meta [] {:sneaky (atom 1)})))
    "WideDraft: the child field :parts holds a vector with the metadata" "(with-meta v nil)"]
   [:mutable-value #(pt/parse (->DebitDraft 1 2 (atom 1))) "the field :amount holds an atom (clojure.lang.Atom)"
    "Deref it, as in @a."]
   [:value-too-deep #(pt/parse (->DebitDraft 1 2 (reduce (fn [x _] [x]) 1 (range 1001))))
    "the field :amount at [0 0 0 0 0 0 0 0] and 993 levels more holds a value nested deeper than 1000 levels"
    "(vec (flatten v))" "(pr-str v)"]
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
   [:got-plain-map #(pt/parsed Debit {:amount 1}) "got a plain map" "(pt/parse (->DebitDraft ...))"]
   [:got-other-type #(pt/parsed Debit (pt/parse (->CreditDraft 1))) "got a Credit" "(pt/parse (->DebitDraft ...))"]
   [:draft-of-a-draft #(pt/draft-of draft) "got a DebitDraft, which is already a draft" "Use the draft as it is"]
   [:draft-of-unissued #(pt/draft-of (Debit. 1 2 30M nil)) "got a Debit that pt/parse did not issue" "(->DebitDraft ...)"]
   [:match-compiled-before-member
    ;; what a match on Growing compiled when it held only Debit does once Credit joins
    #((var parsed-types.core/match-fell-through!) (#'pt/sum 'Growing [Debit Credit]) [Debit] 'parsed-types.errors-test
      'parsed-types.errors-test (pt/parse (->CreditDraft 1)))
    "got a parsed Credit, but this match was compiled before Credit joined the sum" "Re-evaluate the match"]
   ;; a REPL re-evaluated a defparsed, and code evaluated before it keeps the old class
   [:stale-value (in-ns-named 'errs.stale-value '(do (pt/defparsed V [x])
                                                      (def old (pt/parse (->VDraft 1)))
                                                      (pt/defparsed V [x])
                                                      (pt/parsed V old)))
    "pt/parsed got a parsed V of errs.stale_value.V from before the defparsed V in errs.stale-value was re-evaluated"
    "(pt/parse (->VDraft ...))"]
   [:stale-sink (in-ns-named 'errs.stale-sink '(do (pt/defparsed S [x])
                                                    (defn sink [v] (pt/parsed S v))
                                                    (pt/defparsed S [x])
                                                    (sink (pt/parse (->SDraft 1)))))
    "the code calling (pt/parsed S ...) was compiled before the defparsed S in errs.stale-sink was re-evaluated"
    "Re-evaluate the namespace that calls pt/parsed" "(clojure.tools.namespace.repl/refresh)"]
   [:stale-child-type (in-ns-named 'errs.stale-child '(do (pt/defparsed K [x])
                                                           (pt/defparsed P [ks] (children ks K))
                                                           (pt/defparsed K [x])
                                                           (pt/parse (->PDraft [(->KDraft 1)]))))
    "PDraft: the child field :ks holds a KDraft of errs.stale_child.K"
    "the defparsed P in errs.stale-child was evaluated against an older errs.stale_child.K"
    "Re-evaluate the defparsed P, or reload its namespace, as in (require 'errs.stale-child :reload)"]
   [:stale-match (in-ns-named 'errs.stale-match '(do (pt/defparsed M1 [x])
                                                      (pt/defparsed M2 [x])
                                                      (pt/defsum MS [M1 M2])
                                                      (defn f [v] (pt/match MS v M1 a 1 M2 b 2))
                                                      (pt/defparsed M1 [x])
                                                      (f (pt/parse (->M1Draft 1)))))
    "match on MS, compiled in the namespace errs.stale-match, got a parsed errs.stale_match.M1"
    "(require 'errs.stale-match :reload)"]
   [:stale-sum (in-ns-named 'errs.stale-sum '(do (pt/defparsed N1 [x])
                                                  (pt/defparsed N2 [x])
                                                  (pt/defsum NS [N1 N2])
                                                  (pt/defparsed N1 [x])
                                                  (fn [v] (pt/match NS v N1 a 1 N2 b 2))))
    "the clause N1 names errs.stale_sum.N1, but the sum holds another class of that name"
    "Re-evaluate the defsum NS after the defparsed N1, then this match"]
   [:class-name-other-role (in-ns-named 'errs.roles '(do (pt/defparsed FooDraft [x]) (pt/defparsed Foo [y])))
    "errs.roles.FooDraft is already the parsed type of (defparsed FooDraft ...) in errs.roles"
    "Rename one of the types, as in (defparsed Foo2 ...)"]
   ;; pt/draft
   [:draft-not-a-literal-map (compiling '(fn [m] (pt/draft Debit m))) "pt/draft: m is not a literal map"
    "(pt/draft Debit {:user-id ..., :account-id ..., :amount ...})" "(->DebitDraft ...)"]
   [:draft-type-not-a-symbol (compiling '(pt/draft "Credit" {:amount 1})) "the type \"Credit\" is not a symbol"
    "(pt/draft Credit {:amount 1})"]
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

(deftype Fake [amount]
  parsed_types.core.ParsedValue
  (issuedWith [_ _] true))

(deftest only-sum-builds-a-sum-test
  (testing "the factory is removed, so a Sum with members that are not parsed types is not easy to build"
    (is (nil? (ns-resolve 'parsed-types.core '->Sum)))
    (is (nil? (ns-resolve 'parsed-types.core 'map->Sum)))
    (doseq [members [[String] 5 [] [Debit Debit] [Fake]]]
      (is (= :parsed-types.rule/bad-sum-members (:rule (error #(#'pt/sum 'S members)))) (pr-str members))))
  (testing "a sum is not a map, so assoc and merge cannot build one from another"
    (is (not (map? Txn)))
    (is (thrown? ClassCastException (assoc Txn :members [Fake]))))
  (testing "a Sum built with its constructor is checked wherever it is used"
    (doseq [members [[Fake] 5 nil [Debit Debit]]]
      (is (= :parsed-types.rule/not-a-parsed-type
             (:rule (error #(pt/parsed? (parsed_types.core.Sum. 'S members) real))))
          (pr-str members)))
    (is (true? (pt/parsed? (parsed_types.core.Sum. 'S [Debit]) real)) "with real members, it is just a sum"))
  (testing "a sum equals a sum with the same name and members, and prints them"
    (is (= Txn (#'pt/sum 'Txn [Debit Credit])))
    (is (= (hash Txn) (hash (#'pt/sum 'Txn [Debit Credit]))))
    (is (not= Txn (#'pt/sum 'Txn [Credit Debit])))
    (is (= "#<Sum Txn [parsed_types.errors_test.Debit parsed_types.errors_test.Credit]>" (pr-str Txn)))))

(deftest parse-all-takes-a-collection-test
  (doseq [x [draft {:amount 1} 5]]
    (is (= :parsed-types.rule/parse-all-not-a-vector (:rule (error #(pt/parse-all x)))) (pr-str x)))
  (is (= 2 (count (pt/parse-all (list draft draft)))) "a list is fine"))

(deftest no-other-throws-test
  (testing "core.clj throws only ex-info: no Java exception escapes without a :type and a :rule"
    (is (nil? (re-seq #"\(throw \((?!ex-info)[^\s)]*" source)))))

(deftest the-type-argument-is-checked-test
  (doseq [[label thunk] [["nil" #(pt/parsed nil real)] ["a keyword" #(pt/parsed? :x real)]
                         ["a draft type" #(pt/parsed DebitDraft real)] ["a bare Sum" #(pt/parsed? (#'pt/sum 'S [Debit]) nil)]
                         ["a class that only implements ParsedValue" #(pt/parsed Fake (Fake. 1))]]]
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

(pt/defparsed Unit [])

;; public: a check below calls it by its qualified name
(defn amount "Doubles `x`." [x] (* 2 x))

(deftest library-fns-refuse-what-is-not-a-library-value-test
  (doseq [[label thunk] [["final! of nil" #(#'pt/final! nil "assoc" nil)]
                         ["fixed! of nil" #(#'pt/fixed! nil "assoc" nil)]
                         ["final! of a number" #(#'pt/final! 5 "assoc" :k)]
                         ["no-field! of nil" #(#'pt/no-field! nil :k "read")]]]
    (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/not-a-library-value}
           (select-keys (ex-data (try (thunk) nil (catch clojure.lang.ExceptionInfo e e))) [:type :rule]))
        label)))

(deftest every-suggested-fix-applies-test
  (testing "a type with no fields says to add one, rather than naming a field it does not have"
    (is (= "Unit has no field :x: it has no fields at all, so every key read without a default throws. Add the field to its defparsed, as in (defparsed Unit [x])."
           (:message (error #(:x (pt/parse (->UnitDraft)))))))
    (is (str/includes? (:message (error #(assoc (->UnitDraft) :x 1))) "(defparsed Unit [x])")))
  (testing "a key that is not a field, on a parsed value, is reported first, so the fix names a field"
    (let [{:keys [type message]} (error #(assoc real :acount-id 1))]
      (is (= ::pt/no-field type))
      (is (= (str "Debit has no field :acount-id. Did you mean :account-id? A parsed value has fixed fields, and never"
                  " changes. Change one of [:user-id :account-id :amount] on a copy, as in (pt/parse (assoc"
                  " (pt/draft-of x) :account-id v)).")
             message)))
    (is (= ::pt/no-field (:type (error #(dissoc real :bogus)))))
    (is (= ::pt/no-field (:type (error #(dissoc draft :bogus)))))
    (testing "and the fix it names parses"
      (is (some? (pt/parse (assoc (pt/draft-of real) :account-id 3))))))
  (testing "a check that uses a field it does not name: each fix the message offers compiles and passes"
    (let [parses? (fn [form draft-form]
                    ((compiling form))
                    (some? (pt/parse ((compiling draft-form)))))]
      (testing "a field named like a clojure.core fn, misread as the fn: caught"
        (let [{:keys [rule message]} (error (compiling '(pt/defparsed Order [items count]
                                                          (pure ::two "Two items." [items] (= 2 (count items))))))]
          (is (= :parsed-types.rule/check-uses-unnamed-field rule))
          (is (str/includes? message (str "`count` is a field of Order and also clojure.core/count. If you meant the"
                                          " field, add it to the argument vector: [items count]. If you meant the"
                                          " function, write clojure.core/count.")))))
      (testing "meant the field: add it to the argument vector"
        (is (parses? '(pt/defparsed Order [items count] (pure ::counted "count counts items." [items count]
                                                               (= count (clojure.core/count items))))
                     '(->OrderDraft [1 2] 2))))
      (testing "meant the core fn: write its qualified name"
        (is (parses? '(pt/defparsed Order [items count] (pure ::two "Two items." [items] (= 2 (clojure.core/count items))))
                     '(->OrderDraft [1 2] 7))))
      (testing "meant a fn of this namespace: the message offers its qualified name, which works"
        (is (str/includes? (:message (error (compiling '(pt/defparsed Priced [amount qty]
                                                          (pure ::small "Small." [qty] (< (amount qty) 10))))))
                           (str "`amount` is a field of Priced and also parsed-types.errors-test/amount. If you meant"
                                " the field, add it to the argument vector: [qty amount]. If you meant the function,"
                                " write parsed-types.errors-test/amount.")))
        (is (parses? '(pt/defparsed Priced [amount qty]
                        (pure ::small "Small." [qty] (< (parsed-types.errors-test/amount qty) 10)))
                     '(->PricedDraft 1 2))))
      (testing "a let-local named like a field is refused, and the message says to rename it"
        (is (str/includes? (:message (error (compiling '(pt/defparsed Pair [a b]
                                                          (pure ::same "a is 1." [a] (let [b 1] (= a b)))))))
                           (str "the check :parsed-types.errors-test/same uses the field `b` without naming it. Add it"
                                " to the argument vector: [a b]. If it is a local, rename the local, as in b-value.")))
        (is (parses? '(pt/defparsed Pair [a b] (pure ::same "a is 1." [a] (let [b-value 1] (= a b-value))))
                     '(->PairDraft 1 2))))
      (testing "a quoted symbol named like a field is refused too: the rule is syntactic"
        (is (= :parsed-types.rule/check-uses-unnamed-field
               (:rule (error (compiling '(pt/defparsed Quoted [x y] (pure ::q "x is y." [x] (= x 'y)))))))))))
  (testing "parse of a value built by hand says it has no proof, and to build a draft"
    (let [{:keys [rule message]} (error #(pt/parse (Debit. 1 2 30M nil)))]
      (is (= :parsed-types.rule/not-a-draft rule))
      (is (str/includes? message "It is a Debit that pt/parse did not issue: a value with no proof. Build a draft and parse that, as in (pt/parse (->DebitDraft ...))."))
      (is (not (str/includes? message "already parsed")))))
  (testing "a type or sum named by something other than a symbol: each fix the message names compiles"
    (doseq [[form fixed?] [['(pt/draft "Credit" {:amount 1}) #(instance? CreditDraft %)]
                           ['(pt/draft :Credit {:amount 1}) #(instance? CreditDraft %)]
                           ['(fn [x] (pt/match "Txn" x Debit d 1 Credit c 2)) fn?]
                           ['(fn [x] (pt/match :Txn x Debit d 1 Credit c 2)) fn?]]]
      (let [{:keys [rule message]} (error (compiling form))
            fix                    (re-find #"\(pt/(?:draft|match) .*\)(?=\.$)" (str message))]
        (is (#{:parsed-types.rule/draft-type-not-a-symbol :parsed-types.rule/match-sum-not-a-symbol} rule) (pr-str form))
        ;; the match is written inside (fn [x] ...), so its fix goes there too
        (is (fixed? ((compiling (cond->> (read-string fix) (= 'fn (first form)) (list 'fn '[x]))))) fix)))
    (testing "and a clause named by a keyword or a string says which symbol to write"
      (doseq [clause [:Debit "Debit"]]
        (let [{:keys [rule message]} (error (compiling (list 'fn '[x] (list 'pt/match 'Txn 'x clause 'd 1 'Credit 'c 2))))]
          (is (= :parsed-types.rule/match-not-a-type rule))
          (is (str/includes? message (str "Write Debit for " (pr-str clause) ", naming one of [Debit Credit].")))))
      (is (fn? ((compiling '(fn [x] (pt/match Txn x Debit d 1 Credit c 2))))))))
  (testing "parse of a value parse issued says it is already parsed"
    (is (str/includes? (:message (error #(pt/parse real))) "It is already parsed: pass it as it is"))))

(deftest the-public-api-is-what-users-call-test
  (testing "the fns only generated code calls are private, so the public names are the README's API"
    (is (= '#{declaration defparsed defsum draft draft-of match parse parse-all parsed parsed?}
           (set (keys (ns-publics 'parsed-types.core)))))))

(deftest suggestions-are-data-too-test
  (is (= :amount (:suggestion (ex-data (try (:amont real) (catch clojure.lang.ExceptionInfo e e))))))
  (is (= :amount (:suggestion (ex-data (try ((compiling '(pt/draft Debit {:user-id 1, :account-id 2, :amont 3}))) nil
                                             (catch Exception e (ex-cause e)))))))
  (testing "no suggestion when nothing is close, or the key is short"
    (is (not (contains? (ex-data (try (:zzzzzz real) (catch clojure.lang.ExceptionInfo e e))) :suggestion)))
    (is (not (contains? (ex-data (try (:x real) (catch clojure.lang.ExceptionInfo e e))) :suggestion)))))
