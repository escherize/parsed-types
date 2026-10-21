(ns parsed-types.defparsed-test
  "What `defparsed` refuses at compile time, and the declaration it writes."
  (:require
   [clojure.string :as str]
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
            #"uses the field `b` without naming it\. Add it to the argument vector: \[a b\]"]
           ["a check that uses a field named like a clojure.core fn, without naming it"
            '(pt/defparsed A [a name] (pure ::c "d" [a] (string? (name a))))
            #"`name` is a field of A and also clojure.core/name\. If you meant the field, add it to the argument vector: \[a name\]\. If you meant the function, write clojure.core/name\."]
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
           ["a name used twice"
            '(pt/defparsed A [a] (pure ::c "d" [a] true) (pure ::c "d" [a] true))
            #"used twice"]
           ["a field named twice"
            '(pt/defparsed A [a a])
            #"field is named twice"]
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
            #"A clause is \(pure"]
           ["a children clause, which does not exist: a field holds parsed values"
            '(pt/defparsed A [kids] (children kids Point))
            #"children does not start a clause"]]]
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
    (let [decl (pt/declaration (pt/draft Point {:x 1, :y 1}))]
      (is (= Point (:parses-to decl)))
      (is (= [{:name ::x-int, :doc "x is an integer.", :kind :pure, :reads [:x]}
              {:name ::y-int, :doc "y is an integer.", :kind :pure, :reads [:y]}
              {:name ::on-diagonal, :doc "x equals y.", :kind :pure, :reads [:x :y]}
              {:name ::known, :doc "The point is known.", :kind :external, :reads [], :fetches [::row]}]
             (mapv #(dissoc % :f) (:checks decl))))
      (is (= [{:name ::row, :reads [:x]}] (mapv #(dissoc % :f) (:fetches decl))))))
  (testing "and parses with it"
    (is (= {:x 2, :y 2} (into {} (pt/parse (pt/draft Point {:x 2, :y 2})))))
    (is (= :pure (:kind (try (pt/parse (pt/draft Point {:x 1, :y :y})) (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(pt/defparsed Other [x z])

(pt/defsum Shape [Point Other])

(deftest only-defparsed-makes-types-test
  (testing "the macros that defined a parsed type or a draft type alone are private, so calling them does not compile"
    (is (re-find #"defdraft-type is not public" (str (compile-error '(pt/defdraft-type HandDraft [a] {})))))
    (is (re-find #"defparsed-type is not public" (str (compile-error '(pt/defparsed-type Hand [a])))))))

(defn- resolve-here [sym] @(ns-resolve 'parsed-types.defparsed-test sym))

(defn- resolve-here-class [sym] (ns-resolve 'parsed-types.defparsed-test sym))

(defn- thrown
  "The ex-data of what `thunk` threw, with its message as `:message`, through compiler wrapping, or nil."
  [thunk]
  (try (thunk) nil
       (catch Throwable t
         (when-let [e (loop [e t] (cond (nil? e) nil, (:type (ex-data e)) e, :else (recur (ex-cause e))))]
           (assoc (ex-data e) :message (ex-message e))))))

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
              #"the fetch :parsed-types.defparsed-test/f is 42, not a fn"]]]
      (testing label
        (let [[data message] (load-error form)]
          (is (= ::pt/bad-declaration (:type data)))
          (is (re-find re (str message)))))))
  (testing "a defparsed that fails when it loads changes nothing: the classes and the plan stay the old ones"
    (eval-here '(pt/defparsed Kept [a] (pure ::kept-pos "a is positive." [a] (pos? a))))
    (let [cls     (resolve-here-class 'Kept)
          dcls    (resolve-here-class 'KeptDraft)
          old     (pt/parse (eval-here '(pt/draft Kept {:a 1})))]
      (doseq [[label form] [["a fetch that is not a fn" '(pt/defparsed Kept [a] (fetch ::f [a] 42))]]]
        (testing label
          (is (= ::pt/bad-declaration (:type (first (load-error form)))))
          (is (identical? cls (resolve-here-class 'Kept)))
          (is (identical? dcls (resolve-here-class 'KeptDraft)))
          (is (identical? old (pt/parsed (resolve-here-class 'Kept) old)))
          (is (= ::kept-pos (:check (thrown #(pt/parse (eval-here '(pt/draft Kept {:a -1})))))))))))
  (testing "a defparsed that fails after its deftypes, as when a check does not compile, says so wherever its type is used"
    (eval-here '(pt/defparsed Broken [a]))
    (is (some? (load-error '(pt/defparsed Broken [a] (pure ::b "b" [a] (no-such-fn a))))))
    ;; pt/draft refuses the type, so the draft is built with its constructor
    (let [draft (eval-here '(parsed_types.defparsed_test.BrokenDraft. 1))]
      (doseq [[label thunk] [["parse" #(pt/parse draft)]
                             ["parsed" #(pt/parsed (resolve-here-class 'Broken) nil)]
                             ["draft" #(eval-here '(pt/draft Broken {:a 1}))]]]
        (testing label
          (let [e (thrown thunk)]
            (is (= {:type ::pt/bad-declaration, :rule :parsed-types.rule/type-not-loaded}
                   (select-keys e [:type :rule])))
            (is (re-find #"the defparsed Broken in parsed-types.defparsed-test failed to load.*Fix that error and re-evaluate the defparsed Broken"
                         (str (:message e)))))))
      (testing "and the fix it names works"
        (eval-here '(pt/defparsed Broken [a]))
        (is (some? (pt/parse (eval-here '(pt/draft Broken {:a 1})))))))))

(deftest defparsed-only-at-the-top-level-test
  (doseq [form ['(let [x 1] (pt/defparsed InLet [a] (pure ::c "d" [a] (= a x))))
                '(when true (pt/defparsed InWhen [a]))]]
    (let [e (thrown #(eval-here form))]
      (is (= {:type ::pt/bad-declaration, :rule :parsed-types.rule/defparsed-not-top-level}
             (select-keys e [:type :rule])))
      (is (re-find #"Write defparsed at the top level of a namespace" (str (:message e))))))
  (is (nil? (ns-resolve 'parsed-types.defparsed-test 'InLet)) "and it defines no class")
  (testing "the fix it names works: a top-level def in place of the local"
    (eval-here '(def limit 1))
    (eval-here '(pt/defparsed InLet [a] (pure ::c "d" [a] (= a limit))))
    (is (some? (pt/parse (eval-here '(pt/draft InLet {:a 1})))))))

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
    (is (= (pt/draft Point {:x 1, :y nil}) (pt/draft Point {:x 1, :y nil}))))
  (testing "a literal map that leaves a field out does not compile"
    (is (re-find #"Point needs every field; missing \[:y\]" (str (compile-error '(pt/draft Point {:x 1}))))))
  (testing "a literal map with a key that is not a field does not compile"
    (is (re-find #"Point has no fields \[:z\]\. A literal map names only fields; its fields are \[:x :y\]"
                 (str (compile-error '(pt/draft Point {:x 1, :z 2}))))))
  (testing "anything but a literal map does not compile, and the message names the fields to write"
    (is (re-find #"pt/draft: m is not a literal map\..*\(pt/draft Point \{:x \.\.\., :y \.\.\.\}\)\.$"
                 (str (compile-error '(fn [m] (pt/draft Point m))))))
    (is (re-find #"is not a literal map" (str (compile-error '(pt/draft Point (hash-map :x 1 :y 2))))))
    (is (nil? (resolve 'parsed-types.core/draft-from-map)) "there is no run-time path"))
  (testing "a type without a draft does not compile"
    (is (re-find #"not a type defined with defparsed" (str (compile-error '(pt/draft String {})))))))

;;; Keys that are not fields

(deftest unknown-keys-test
  (let [d (pt/draft Point {:x 1, :y 2})
        p (pt/parse (pt/draft Point {:x 2, :y 2}))]
    (doseq [[label x] [["a draft" d] ["a parsed value" p]]]
      (testing label
        (doseq [[how thunk] [["keyword" #(:z x)]
                             ["get" #(get x :z)]
                             ["invoke" #(x :z)]
                             ["destructuring" #(let [{:keys [z]} x] z)]
                             ["get with a default" #(get x :z ::none)]
                             ["get-in with a default" #(get-in x [:z] ::none)]
                             ["keyword with a default" #(:z x ::none)]
                             ["invoke with a default" #(x :z ::none)]
                             ["destructuring with :or" #(let [{:keys [z] :or {z ::none}} x] z)]]]
          (let [data (try (thunk) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
            (is (= ::pt/no-field (:type data)) how)
            (is (= :parsed-types.rule/no-such-field-read (:rule data)) how)))
        (testing "a field read with a default reads its value, nil too"
          (is (= (repeat 5 (:x x)) [(get x :x ::none) (get-in x [:x] ::none) (:x x ::none) (x :x ::none)
                              (let [{:keys [x] :or {x ::none}} x] x)]))
          (is (nil? (:y (assoc d :y nil) ::none))))
        (testing "contains?, find and select-keys probe"
          (is (false? (contains? x :z)))
          (is (nil? (find x :z)))
          (is (= {:x (:x x)} (select-keys x [:x :z]))))
        (testing "java.util.Map keeps Java's contract: get returns null, getOrDefault the default"
          (is (nil? (.get ^java.util.Map x :z)))
          (is (= ::none (.getOrDefault ^java.util.Map x :z ::none)))
          (is (some? (.get ^java.util.Map x :x))))
        (is (= [:x :y] (keys x)))))))

;;; REPL reloads

(deftest re-evaluating-a-type-test
  (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
  (eval-here '(defn- kid-sink [x] (pt/parsed Kid x)))
  (let [old-kid   (eval-here '(pt/draft Kid {:n 1}))
        old-value (pt/parse old-kid)
        sink      (resolve-here 'kid-sink)]
    (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
    (let [new-kid (eval-here '(pt/draft Kid {:n 2}))]
      (is (not= (class old-kid) (class new-kid)) "the reload made a new class")
      (testing "old drafts still parse, with the plans they were defined with, as records would"
        (is (= (class old-value) (class (pt/parse old-kid)))))
      (testing "a sink compiled before the reload says to re-evaluate the code that calls it"
        (let [e (thrown #(sink (pt/parse new-kid)))]
          (is (= :parsed-types.rule/stale-sink (:rule e)))
          (is (re-find #"the code calling \(pt/parsed Kid \.\.\.\) was compiled before the defparsed Kid in parsed-types.defparsed-test was re-evaluated"
                       (:message e)))
          (is (re-find #"\(clojure.tools.namespace.repl/refresh\)" (:message e))))
        (is (identical? old-value (sink old-value)) "and still takes the values of its own class"))
      (eval-here '(defn- kid-sink [x] (pt/parsed Kid x)))
      (testing "once re-evaluated, the sink takes the new class"
        (is (= {:n 2} (into {} ((resolve-here 'kid-sink) (pt/parse new-kid))))))
      (testing "and says to build again what was built before the reload"
        (let [e (thrown #((resolve-here 'kid-sink) old-value))]
          (is (= :parsed-types.rule/stale-value (:rule e)))
          (is (str/includes? (:message e) "(pt/parse (pt/draft Kid {:n ...}))")))
        (is (= :parsed-types.rule/stale-value (:rule (thrown #(pt/parsed? (ns-resolve 'parsed-types.defparsed-test 'Kid) old-value)))))))))

(deftest re-evaluating-a-member-of-a-sum-test
  (eval-here '(pt/defparsed Member [n]))
  (eval-here '(pt/defparsed NonMember [n]))
  (eval-here '(pt/defsum Members [Member NonMember]))
  (eval-here '(defn- which-member [x] (pt/match Members x Member m [:member (:n m)] NonMember o [:non-member (:n o)])))
  (eval-here '(pt/defparsed Member [n]))
  (let [member (pt/parse (eval-here '(pt/draft Member {:n 1})))]
    (testing "the sum names itself, and a match compiled before the reload names its namespace"
      (is (re-find #"Re-evaluate the defsum Members, then the namespace that calls pt/parsed\?"
                   (:message (thrown #(pt/parsed? (resolve-here 'Members) member)))))
      (let [e (thrown #((resolve-here 'which-member) member))]
        (is (= {:rule :parsed-types.rule/stale-match, :ns 'parsed-types.defparsed-test} (select-keys e [:rule :ns])))
        (is (re-find #"match on Members, compiled in the namespace parsed-types.defparsed-test, got a parsed parsed_types.defparsed_test.Member"
                     (:message e)))
        (is (re-find #"\(require 'parsed-types.defparsed-test :reload\)" (:message e)))))
    (testing "a match compiled after the reload, against the old defsum, does not compile"
      (let [e (thrown #(eval-here '(fn [x] (pt/match Members x Member m (:n m) NonMember o (:n o)))))]
        (is (= {:type ::pt/stale-type, :rule :parsed-types.rule/stale-match} (select-keys e [:type :rule])))
        (is (re-find #"Re-evaluate the defsum Members after the defparsed Member, then this match" (:message e)))))
    (eval-here '(pt/defsum Members [Member NonMember]))
    (eval-here '(defn- which-member [x] (pt/match Members x Member m [:member (:n m)] NonMember o [:non-member (:n o)])))
    (testing "once the sum and the match are re-evaluated, they take the new class"
      (is (pt/parsed? (resolve-here 'Members) member))
      (is (= [:member 1] ((resolve-here 'which-member) member))))))

(defn- fx
  "The value of `sym` in the stale-fixture namespace `suffix`, as it is now, as in (fx \"app\" 'transact)."
  [suffix sym]
  @(ns-resolve (symbol (str "parsed-types.stale-fixture." suffix)) sym))

(defn- apply-fix!
  "Evaluate the `(require ... :reload)` the message of `e` names, as written, and return it."
  [e]
  (let [cmd (re-find #"\(require [^)]*:reload\)" (str (:message e)))]
    (is (some? cmd) (str "no command in: " (:message e)))
    (when cmd (eval (read-string cmd)))
    cmd))

(deftest every-reload-command-applies-test
  (let [nss   (fn [& suffixes] (map #(symbol (str "parsed-types.stale-fixture." %)) suffixes))
        debit #(binding [*ns* (the-ns 'parsed-types.stale-fixture.types)] (eval '(pt/draft Debit {:amount 5})))]
    (apply require (concat (nss "types" "sum" "app") [:reload]))
    (require (first (nss "types")) :reload)
    (testing "a match compiled before the reload: reload the sum's namespace, then the match's"
      (let [e (thrown #((fx "app" 'transact) (pt/parse (debit))))]
        (is (= :parsed-types.rule/stale-match (:rule e)))
        (is (= "(require 'parsed-types.stale-fixture.sum 'parsed-types.stale-fixture.app :reload)" (apply-fix! e)))
        (is (= [:debit 5] ((fx "app" 'transact) (pt/parse (debit)))))))
    (testing "a match compiled against a sum from before the reload: the same"
      (require (first (nss "types")) :reload)
      (let [e (thrown #(require (first (nss "app")) :reload))]
        (is (= :parsed-types.rule/stale-match (:rule e)))
        (is (= "(require 'parsed-types.stale-fixture.sum 'parsed-types.stale-fixture.app :reload)" (apply-fix! e)))
        (is (= [:debit 5] ((fx "app" 'transact) (pt/parse (debit)))))))))

(deftest reloading-the-library-keeps-every-type-test
  (eval-here '(pt/defparsed RlLine [n] (pure ::rl-n-int "n is an integer." [n] (int? n))))
  (eval-here '(pt/defparsed RlOther [n]))
  (eval-here '(pt/defsum RlSum [RlLine RlOther]))
  (eval-here '(defn- rl-match [x] (pt/match RlSum x RlLine l [:line (:n l)] RlOther o [:other (:n o)])))
  (let [line-type  (ns-resolve 'parsed-types.defparsed-test 'RlLine)
        interfaces #(mapv (fn [n] (Class/forName n false (clojure.lang.DynamicClassLoader.)))
                          ["parsed_types.core.ParsedValue" "parsed_types.core.DraftValue" "parsed_types.core.Sum"])
        before     (interfaces)
        parsed     (pt/parse (eval-here '(pt/draft RlLine {:n 1})))
        good       (eval-here '(pt/draft RlLine {:n 2}))
        bad        (eval-here '(pt/draft RlLine {:n :x}))]
    (require 'parsed-types.core :reload)
    (testing "the library's classes are the ones loaded before"
      (is (= before (interfaces))))
    (testing "a value parsed before the reload is still parsed"
      (is (pt/parsed? line-type parsed))
      (is (identical? parsed (pt/parsed line-type parsed))))
    (testing "a draft built before the reload still parses, and still fails its checks"
      (is (pt/parsed? line-type (pt/parse good)))
      (is (= ::rl-n-int (:check (try (pt/parse bad) nil
                                     (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
    (testing "a sum and a match defined before the reload still take the value"
      (is (pt/parsed? (resolve-here 'RlSum) parsed))
      (is (= [:line 1] ((resolve-here 'rl-match) parsed))))
    (testing "a type defined after the reload holds a value parsed before it"
      (eval-here '(pt/defparsed RlEntry [lines]))
      (is (identical? parsed (first (:lines (pt/parse ((eval-here '(fn [x] (pt/draft RlEntry {:lines [x]}))) parsed)))))))))

(deftest a-hand-written-class-with-the-same-name-is-refused-test
  (eval-here '(pt/defparsed Genuine [n]))
  (let [genuine (ns-resolve 'parsed-types.defparsed-test 'Genuine)
        real    (pt/parse (eval-here '(pt/draft Genuine {:n 1})))]
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

(defn- loaded-class [class-name] (Class/forName class-name false (clojure.lang.DynamicClassLoader.)))

(deftest a-class-name-belongs-to-one-namespace-test
  (eval-in 'pt-owner.my-app.bank '(pt/defparsed Debit [amount] (pure ::pos "The amount is positive." [amount] (pos? amount))))
  (let [real-type  (eval-in 'pt-owner.my-app.bank 'Debit)
        real-draft (eval-in 'pt-owner.my-app.bank 'DebitDraft)
        sink       (eval-in 'pt-owner.my-app.bank '(fn [d] (:amount (pt/parsed Debit d))))]
    (testing "a namespace whose name compiles to the same Java package cannot define a type of that name"
      (let [e (thrown #(eval-in 'pt-owner.my_app.bank '(pt/defparsed Debit [amount])))]
        (is (= {:type  ::pt/bad-declaration, :rule :parsed-types.rule/class-name-taken
                :class "pt_owner.my_app.bank.Debit", :owner 'pt-owner.my-app.bank, :ns 'pt-owner.my_app.bank}
               (select-keys e [:type :rule :class :owner :ns])))
        (is (re-find #"already defined by a defparsed in the namespace pt-owner.my-app.bank" (:message e)))
        (is (re-find #"Rename one of the namespaces" (:message e)))))
    (testing "inside a let too, which is refused before it compiles"
      (is (= :parsed-types.rule/defparsed-not-top-level
             (:rule (thrown #(eval-in 'pt-owner.my_app.bank '(let [] (pt/defparsed Debit [amount]))))))))
    (testing "and the refused defparsed defined no class: the owner's classes are still the loaded ones"
      (is (identical? real-type (loaded-class "pt_owner.my_app.bank.Debit")))
      (is (identical? real-draft (loaded-class "pt_owner.my_app.bank.DebitDraft")))
      (is (nil? (ns-resolve (the-ns 'pt-owner.my_app.bank) 'DebitDraft)))
      (is (= 5 (sink (eval-in 'pt-owner.my-app.bank '(pt/parse (pt/draft Debit {:amount 5})))))))
    (testing "the owning namespace may redefine it, as a REPL reload does"
      (eval-in 'pt-owner.my-app.bank '(pt/defparsed Debit [amount] (pure ::pos "The amount is positive." [amount] (pos? amount))))
      (let [sink (eval-in 'pt-owner.my-app.bank '(fn [d] (:amount (pt/parsed Debit d))))]
        (is (= 5 (sink (eval-in 'pt-owner.my-app.bank '(pt/parse (pt/draft Debit {:amount 5}))))))))))

(deftest a-class-name-has-one-role-test
  (testing "a type named like another type's draft is refused, in either order, before it defines a class"
    (eval-in 'pt-roles.a '(pt/defparsed FooDraft [x]))
    (let [parsed (loaded-class "pt_roles.a.FooDraft")
          e      (thrown #(eval-in 'pt-roles.a '(pt/defparsed Foo [y])))]
      (is (= {:type ::pt/bad-declaration, :rule :parsed-types.rule/class-name-other-role, :class "pt_roles.a.FooDraft"
              :role :draft, :owner-role :parsed}
             (select-keys e [:type :rule :class :role :owner-role])))
      (is (re-find #"pt_roles.a.FooDraft is already the parsed type of \(defparsed FooDraft \.\.\.\) in pt-roles.a"
                   (:message e)))
      (is (re-find #"Rename one of the types, as in \(defparsed Foo2 \.\.\.\)" (:message e)))
      (is (identical? parsed (loaded-class "pt_roles.a.FooDraft")) "FooDraft is still the parsed class"))
    (eval-in 'pt-roles.b '(pt/defparsed Bar [y]))
    (let [draft (loaded-class "pt_roles.b.BarDraft")
          e     (thrown #(eval-in 'pt-roles.b '(pt/defparsed BarDraft [])))]
      (is (= {:rule :parsed-types.rule/class-name-other-role, :role :parsed, :owner-role :draft}
             (select-keys e [:rule :role :owner-role])))
      (is (identical? draft (loaded-class "pt_roles.b.BarDraft")) "BarDraft is still Bar's draft class")
      (is (some? (pt/parse (eval-in 'pt-roles.b '(pt/draft Bar {:y 1}))))))))

(deftest a-match-compiled-before-a-member-joined-throws-test
  (eval-here '(pt/defparsed MA [x]))
  (eval-here '(pt/defparsed MB [x]))
  (eval-here '(pt/defparsed MC [x]))
  (eval-here '(pt/defsum Grows [MA MB]))
  (eval-here '(defn- grows-match [v] (pt/match Grows v MA a [:a a] MB b [:b b])))
  (eval-here '(pt/defsum Grows [MA MB MC]))
  (let [e (try ((resolve-here 'grows-match) (pt/parse (eval-here '(pt/draft MC {:x 1})))) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type ::pt/not-parsed, :rule :parsed-types.rule/match-compiled-before-member, :handled '[MA MB]}
           (select-keys (ex-data e) [:type :rule :handled])))
    (is (re-find #"compiled before MC joined the sum.*Re-evaluate the match" (ex-message e))))
  (testing "values that are not parsed still get the sink's own error"
    (is (= :parsed-types.rule/got-draft
           (:rule (try ((resolve-here 'grows-match) (eval-here '(pt/draft MC {:x 1}))) nil
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

;;; Field names

(pt/defparsed Awkward [count map set str seq into contains? or when keys]
  (pure ::count-int "count is an integer." [count] (int? count)))

(deftest fields-named-like-core-functions-test
  (testing "a field may be named like any clojure.core function or macro"
    (let [d (pt/draft Awkward {:count 1, :map 2, :set 3, :str 4, :seq 5, :into 6, :contains? 7, :or 8, :when 9, :keys 10})
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

(def ^:private order (atom []))

(pt/defparsed Nine [a b c d e f g h i])

(defn- logged [k] (swap! order conj k) k)

(deftest draft-evaluates-a-literal-map-in-the-order-written-test
  (testing "up to 8 keys, in the order written"
    (reset! order [])
    (pt/draft Point {:y (do (swap! order conj :y) 2), :x (do (swap! order conj :x) 1)})
    (is (= [:y :x] @order)))
  (testing "with more, in the order Clojure evaluates the same map literal, which the reader no longer keeps"
    (reset! order [])
    (pt/draft Nine {:i (logged :i) :h (logged :h) :g (logged :g) :f (logged :f) :e (logged :e) :d (logged :d)
                    :c (logged :c) :b (logged :b) :a (logged :a)})
    (let [drafted @order]
      (reset! order [])
      (is (map? {:i (logged :i) :h (logged :h) :g (logged :g) :f (logged :f) :e (logged :e) :d (logged :d)
                 :c (logged :c) :b (logged :b) :a (logged :a)}))
      (is (= @order drafted))
      (is (not= [:i :h :g :f :e :d :c :b :a] drafted) "so not the order written"))))

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
    (is (not= (Money. 1M 2 nil) (pt/draft Money {:amount 1M, :count 2})))
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

(deftest every-java-map-mutator-throws-ex-info-test
  (let [f2   (reify java.util.function.BiFunction (apply [_ _ _] 9M))
        f1   (reify java.util.function.Function (apply [_ _] 9M))
        keep (reify java.util.function.Predicate (test [_ _] true))
        op   (fn [thunk] (select-keys (try (thunk) nil (catch clojure.lang.ExceptionInfo e (ex-data e))) [:type :op]))]
    (doseq [[label ^java.util.Map m] [["a parsed value" (pt/parse (pt/draft Money {:amount 1M, :count 2}))] ["a draft" (pt/draft Money {:amount 1M, :count 2})]]]
      (testing label
        (testing "the default methods of java.util.Map"
          (doseq [[name thunk] [["replaceAll" #(.replaceAll m f2)]
                                ["compute" #(.compute m :amount f2)]
                                ["computeIfAbsent" #(.computeIfAbsent m :amount f1)]
                                ["computeIfPresent" #(.computeIfPresent m :amount f2)]
                                ["merge" #(.merge m :amount 9M f2)]
                                ["putIfAbsent" #(.putIfAbsent m :amount 9M)]
                                ["replace" #(.replace m :amount 9M)]
                                ["replace" #(.replace m :amount 1M 9M)]
                                ["remove" #(.remove m :amount 1M)]]]
            (is (= {:type ::pt/cannot-change, :op name} (op thunk)) name)))
        (testing "the views, their iterators and their entries"
          (doseq [[name thunk] [["keySet().clear" #(.clear (.keySet m))]
                                ["keySet().remove" #(.remove (.keySet m) :amount)]
                                ["keySet().add" #(.add (.keySet m) :x)]
                                ["keySet().removeIf" #(.removeIf (.keySet m) keep)]
                                ["keySet().iterator().remove" #(let [it (.iterator (.keySet m))] (.next it) (.remove it))]
                                ["values().remove" #(.remove (.values m) 2)]
                                ["values().retainAll" #(.retainAll (.values m) [])]
                                ["values().addAll" #(.addAll (.values m) [1])]
                                ["values().clear" #(.clear (.values m))]
                                ["entrySet().removeAll" #(.removeAll (.entrySet m) (vec (.entrySet m)))]
                                ["entrySet().iterator().remove" #(let [it (.iterator (.entrySet m))] (.next it) (.remove it))]
                                ["setValue" #(.setValue ^java.util.Map$Entry (first (.entrySet m)) 9M)]]]
            (is (= {:type ::pt/cannot-change, :op name} (op thunk)) name)))
        (testing "while the views read like a java.util.HashMap's"
          (let [h (java.util.HashMap. {:amount 1M, :count 2})]
            (is (= (.keySet h) (.keySet m)))
            (is (= #{:amount :count} (.keySet m)))
            (is (= (.entrySet h) (.entrySet m)))
            (is (= (.hashCode (.entrySet h)) (.hashCode (.entrySet m))))
            (is (= (set (.values h)) (set (.values m))))
            (is (.contains (.values m) 2))
            (is (= 2 (count (.toArray (.values m) (object-array 0)))))))))))

(deftest conj-test
  (let [d (pt/draft Money {:amount 1M, :count 2})]
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
    (let [d ((eval-here '(fn [v] (pt/draft Shadow {:ShadowDraft-declaration v})))
             {:parses-to (resolve 'parsed-types.defparsed-test/Shadow), :checks []})]
      (is (= ::never (:check (try (pt/parse d) (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

(deftest field-name-limits-test
  (doseq [[fields re] [['[a.b] #"cannot contain a dot"]
                       ['[a-b a_b] #"compile to the same Java name"]
                       ['[__meta] #"deftype reserves these field names"]
                       ['[parse-proof] #"reserves the field name parse-proof"]
                       ['[parse_proof] #"reserves the field name parse-proof"]]]
    (is (re-find re (str (compile-error (list 'pt/defparsed 'Limits fields)))))))
