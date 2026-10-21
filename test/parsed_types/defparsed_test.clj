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
           ["a check that reads a field the type does not have"
            '(pt/defparsed A [a] (pure ::c "d" [b] true))
            #"reads fields the type does not have: \[b\]"]
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
           ["a fetch over a field the type does not have"
            '(pt/defparsed A [a] (fetch ::f [b] identity))
            #"fields of the type"]
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
    (let [decl (pt/declaration Point)]
      (is (= Point (:parses-to decl)))
      (is (= [{:name ::x-int, :doc "x is an integer.", :kind :pure, :reads [:x]}
              {:name ::y-int, :doc "y is an integer.", :kind :pure, :reads [:y]}
              {:name ::on-diagonal, :doc "x equals y.", :kind :pure, :reads [:x :y]}
              {:name ::known, :doc "The point is known.", :kind :external, :reads [], :fetches [::row]}]
             (mapv #(dissoc % :f) (:checks decl))))
      (is (= [{:name ::row, :reads [:x]}] (mapv #(dissoc % :f) (:fetches decl))))))
  (testing "and parses with it"
    (is (= {:x 2, :y 2} (pt/parsed Point (pt/parse Point {:x 2, :y 2}))))
    (is (= :pure (:kind (try (pt/parse Point {:x 1, :y :y}) (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(pt/defparsed Other [x z])

(pt/defsum Shape [Point Other])

(deftest only-defparsed-makes-types-test
  (testing "the fn that defines a parsed type alone is private, so calling it does not compile"
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
          old     (eval-here '(pt/parse Kept {:a 1}))]
      (doseq [[label form] [["a fetch that is not a fn" '(pt/defparsed Kept [a] (fetch ::f [a] 42))]]]
        (testing label
          (is (= ::pt/bad-declaration (:type (first (load-error form)))))
          (is (identical? cls (resolve-here-class 'Kept)))
          (is (pt/parsed? (resolve-here-class 'Kept) old))
          (is (= ::kept-pos (:check (thrown #(eval-here '(pt/parse Kept {:a -1}))))))))))
  (testing "a defparsed that fails after its deftypes, as when a check does not compile, says so wherever its type is used"
    (eval-here '(pt/defparsed Broken [a]))
    (is (some? (load-error '(pt/defparsed Broken [a] (pure ::b "b" [a] (no-such-fn a))))))
    (let [broken (resolve-here-class 'Broken)]
      (doseq [[label thunk] [["parse" #(pt/parse broken {:a 1})]
                             ["parsed" #(pt/parsed broken nil)]
                             ["parse, compiled" #(eval-here '(pt/parse Broken {:a 1}))]]]
        (testing label
          (let [e (thrown thunk)]
            (is (= {:type ::pt/bad-declaration, :rule :parsed-types.rule/type-not-loaded}
                   (select-keys e [:type :rule])))
            (is (re-find #"the defparsed Broken in parsed-types.defparsed-test failed to load.*Fix that error and re-evaluate the defparsed Broken"
                         (str (:message e)))))))
      (testing "and the fix it names works"
        (eval-here '(pt/defparsed Broken [a]))
        (is (some? (eval-here '(pt/parse Broken {:a 1}))))))))

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
    (is (some? (eval-here '(pt/parse InLet {:a 1}))))))

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

;;; The input map

(deftest parse-checks-a-literal-map-when-it-compiles-test
  (testing "a literal map that leaves a field out does not compile"
    (is (re-find #"Point needs every field; missing \[:y\]" (str (compile-error '(pt/parse Point {:x 1})))))
    (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/input-missing-keys, :missing [:y]}
           (select-keys (thrown #(eval-here '(pt/parse Point {:x 1}))) [:type :rule :missing]))))
  (testing "a literal map with a key that is not a field does not compile, and names the field it is close to"
    (is (re-find #"Point has no fields \[:yy\]\. Did you mean :y\? A map to parse names only fields; its fields are \[:x :y\]"
                 (str (compile-error '(pt/parse Point {:x 1, :yy 2})))))
    (is (= {:rule :parsed-types.rule/input-unknown-keys, :unknown [:yy], :suggestion :y}
           (select-keys (thrown #(eval-here '(pt/parse Point {:x 1, :yy 2}))) [:rule :unknown :suggestion]))))
  (testing "a map that is not literal compiles, and is checked when it runs"
    (is (nil? (compile-error '(fn [m] (pt/parse Point m)))))))

(deftest parse-checks-any-map-when-it-runs-test
  (let [parse (fn [m] (thrown #(pt/parse Point m)))]
    (testing "a key that is not a field throws, never parses, and names the field it is close to"
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/input-unknown-keys, :fn "pt/parse", :unknown [:yy]
              :suggestion :y, :fields [:x :y], :parsed "parsed_types.defparsed_test.Point"}
             (dissoc (parse {:x 1, :yy 2}) :message))))
    (testing "a missing field throws, never reads nil"
      (is (= {:rule :parsed-types.rule/input-missing-keys, :missing [:y]} (select-keys (parse {:x 1}) [:rule :missing])))
      (is (re-find #"Write nil for a field you mean to leave empty, as in \(pt/parse Point \{:x \.\.\., :y \.\.\.\}\)"
                   (:message (parse {:x 1})))))
    (testing "a field written as nil is present, and reaches the checks"
      (is (= ::y-int (:check (parse {:x 1, :y nil})))))
    (testing "anything but a map throws"
      (is (= :parsed-types.rule/input-not-a-map (:rule (parse [[:x 1] [:y 1]]))))
      (is (= :parsed-types.rule/input-not-a-map (:rule (parse nil)))))
    (testing "a parsed value is already parsed"
      (let [e (parse (pt/parse Point {:x 2, :y 2}))]
        (is (= :parsed-types.rule/already-parsed (:rule e)))
        (is (re-find #"\(pt/parse Point \(assoc \(pt/parsed Point x\) :field v\)\)" (:message e)))))
    (testing "a sum is no type to parse into"
      (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/not-a-parsed-type}
             (select-keys (thrown #(pt/parse Shape {:x 1, :y 1})) [:type :rule]))))
    (testing "parse is a fn, so it maps"
      (is (= [{:x 2, :y 2} {:x 3, :y 3}]
             (mapv #(pt/parsed Point %) (map #(pt/parse Point %) [{:x 2, :y 2} {:x 3, :y 3}])))))
    (testing "a changed copy is a changed map, parsed again"
      (let [p (pt/parse Point {:x 2, :y 2})]
        (is (= {:x 3, :y 3} (pt/parsed Point (pt/parse Point (assoc (pt/parsed Point p) :x 3 :y 3)))))))))

(deftest parse-all-checks-each-map-test
  (testing "a map with the wrong keys throws with its index, before any check runs"
    (is (= {:type ::pt/bad-input, :rule :parsed-types.rule/input-missing-keys, :index 1, :missing [:y]}
           (select-keys (thrown #(pt/parse-all Point [{:x 2, :y 2} {:x 3}])) [:type :rule :index :missing])))
    (is (re-find #"missing \[:y\] in the map at index 1.*\(pt/parse-all Point \[\{:x \.\.\., :y \.\.\.\}\]\)"
                 (:message (thrown #(pt/parse-all Point [{:x 2, :y 2} {:x 3}]))))))
  (testing "a failed check names its index"
    (is (= {:check ::on-diagonal, :index 1}
           (select-keys (thrown #(pt/parse-all Point [{:x 2, :y 2} {:x 3, :y 4}])) [:check :index]))))
  (testing "a single map, not a vector, throws"
    (is (= :parsed-types.rule/parse-all-not-a-vector (:rule (thrown #(pt/parse-all Point {:x 2, :y 2})))))))

;;; Reads: a parsed value is read only through pt/parsed

(deftest every-read-throws-must-unwrap-test
  (let [p (pt/parse Point {:x 2, :y 2})]
    (doseq [[label x rule fix] [["a parsed value" p :parsed-types.rule/read-through-parsed "(pt/parsed Point x)"]]]
      (testing label
        (doseq [[how op k thunk] [["keyword" "get" :x #(:x x)]
                                  ["keyword, not a field" "get" :z #(:z x)]
                                  ["get" "get" :x #(get x :x)]
                                  ["get-in" "get" :x #(get-in x [:x])]
                                  ["invoke" "get" :x #(x :x)]
                                  ["invoke, no argument" "invoke" nil #(x)]
                                  ["destructuring" "get" :x #(let [{:keys [x]} x] x)]
                                  ["get with a default" "get" :z #(get x :z ::none)]
                                  ["keyword with a default" "get" :x #(:x x ::none)]
                                  ["invoke with a default" "get" :x #(x :x ::none)]
                                  ["destructuring with :or" "get" :x #(let [{:keys [x] :or {x ::none}} x] x)]
                                  ["contains?" "contains?" :x #(contains? x :x)]
                                  ["find" "find" :x #(find x :x)]
                                  ["select-keys" "find" :x #(select-keys x [:x])]
                                  ["count" "count" nil #(count x)]
                                  ["seq" "seq" nil #(seq x)]
                                  ;; Clojure 1.12's empty? counts a Counted; 1.11's calls seq
                                  ["empty?" #{"count" "seq"} nil #(empty? x)]
                                  ["into {}" "seq" nil #(into {} x)]
                                  ["keys" "seq" nil #(keys x)]
                                  ["vals" "seq" nil #(vals x)]]]
          (let [e (thrown thunk)]
            (is (= ::pt/must-unwrap (:type e)) how)
            (is (= rule (:rule e)) how)
            (is (if (set? op) (contains? op (:op e)) (= op (:op e))) how)
            (is (= k (:key e)) how)
            (is (str/includes? (str (:message e)) fix) (str how ": the message names the fix"))))))))

;; A sink that forgets pt/parsed: it reads its argument as a map
(defn- forgetful-sink [point]
  (let [{:keys [x y]} point]
    [x y]))

(deftest a-sink-that-skips-parsed-fails-on-real-values-test
  (testing "it takes a plain map, which no library can stop"
    (is (= [1 1] (forgetful-sink {:x 1, :y 1}))))
  (testing "but throws on the first parsed value, so it cannot ship working"
    (is (= ::pt/must-unwrap (:type (thrown #(forgetful-sink (pt/parse Point {:x 1, :y 1}))))))))

(pt/defparsed Segment [from to]
  (pure ::points "Both ends are parsed points." [from to] (and (pt/parsed? Point from) (pt/parsed? Point to))))

(deftest parsed-returns-a-plain-map-test
  (let [a (pt/parse Point {:x 1, :y 1})
        b (pt/parse Point {:x 2, :y 2})
        m (pt/parsed Segment (pt/parse Segment {:from a, :to b}))]
    (testing "pt/parsed returns the fields as a plain map, which carries no proof"
      (is (= {:x 1, :y 1} (pt/parsed Point a)))
      (is (map? (pt/parsed Point a)))
      (is (= clojure.lang.PersistentArrayMap (class (pt/parsed Point a))))
      (is (= ::pt/not-parsed (:type (thrown #(pt/parsed Point (pt/parsed Point a)))))))
    (testing "a field that holds parsed values holds them as they are"
      (is (identical? a (:from m)))
      (is (pt/parsed? Point (:to m)))
      (is (= ::pt/must-unwrap (:type (thrown #(:x (:from m)))))))))

(pt/defparsed Pair [a b]
  (pure ::a-is-x "a's x is 1." [a] (= 1 (:x a))))

(deftest a-check-that-skips-parsed-names-the-fix-test
  (let [e (thrown #(pt/parse Pair {:a (pt/parse Point {:x 1, :y 1}), :b nil}))]
    (is (= ::pt/invalid (:type e)))
    (is (= ::pt/must-unwrap (:type (ex-data (:threw e)))))
    (is (str/includes? (:message e) "read a parsed value without pt/parsed"))
    (is (str/includes? (:message e) "(pt/parsed Point x)"))
    (is (not (str/includes? (:message e) "Guard its input")) "no guard advice: the input is fine")
    (is (not (re-find #"[^.]\.\.(?!\.)" (:message e))) "no doubled period")))

;;; REPL reloads

(deftest re-evaluating-a-type-test
  (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
  (eval-here '(defn- kid-sink [x] (pt/parsed Kid x)))
  (let [old-type  (resolve-here-class 'Kid)
        old-value (eval-here '(pt/parse Kid {:n 1}))
        sink      (resolve-here 'kid-sink)]
    (eval-here '(pt/defparsed Kid [n] (pure ::n-int "n is an integer." [n] (int? n))))
    (let [new-value (eval-here '(pt/parse Kid {:n 2}))]
      (is (not= (class old-value) (class new-value)) "the reload made a new class")
      (testing "the old class still parses, with the plan it was defined with, as records would"
        (is (= (class old-value) (class (pt/parse old-type {:n 3})))))
      (testing "a sink compiled before the reload says to re-evaluate the code that calls it"
        (let [e (thrown #(sink new-value))]
          (is (= :parsed-types.rule/stale-sink (:rule e)))
          (is (re-find #"the code calling \(pt/parsed Kid \.\.\.\) was compiled before the defparsed Kid in parsed-types.defparsed-test was re-evaluated"
                       (:message e)))
          (is (re-find #"\(clojure.tools.namespace.repl/refresh\)" (:message e))))
        (is (= {:n 1} (sink old-value)) "and still takes the values of its own class"))
      (eval-here '(defn- kid-sink [x] (pt/parsed Kid x)))
      (testing "once re-evaluated, the sink takes the new class"
        (is (= {:n 2} ((resolve-here 'kid-sink) new-value))))
      (testing "and says to build again what was built before the reload"
        (let [e (thrown #((resolve-here 'kid-sink) old-value))]
          (is (= :parsed-types.rule/stale-value (:rule e)))
          (is (str/includes? (:message e) "(pt/parse Kid {:n ...})")))
        (is (= :parsed-types.rule/stale-value (:rule (thrown #(pt/parsed? (ns-resolve 'parsed-types.defparsed-test 'Kid) old-value)))))))))

(deftest re-evaluating-a-member-of-a-sum-test
  (eval-here '(pt/defparsed Member [n]))
  (eval-here '(pt/defparsed NonMember [n]))
  (eval-here '(pt/defsum Members [Member NonMember]))
  (eval-here '(defn- which-member [x] (pt/match Members x Member m [:member (:n (pt/parsed Member m))] NonMember o [:non-member (:n (pt/parsed NonMember o))])))
  (eval-here '(pt/defparsed Member [n]))
  (let [member (eval-here '(pt/parse Member {:n 1}))]
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
    (eval-here '(defn- which-member [x] (pt/match Members x Member m [:member (:n (pt/parsed Member m))] NonMember o [:non-member (:n (pt/parsed NonMember o))])))
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
        debit #(binding [*ns* (the-ns 'parsed-types.stale-fixture.types)] (eval '(pt/parse Debit {:amount 5})))]
    (apply require (concat (nss "types" "sum" "app") [:reload]))
    (require (first (nss "types")) :reload)
    (testing "a match compiled before the reload: reload the sum's namespace, then the match's"
      (let [e (thrown #((fx "app" 'transact) (debit)))]
        (is (= :parsed-types.rule/stale-match (:rule e)))
        (is (= "(require 'parsed-types.stale-fixture.sum 'parsed-types.stale-fixture.app :reload)" (apply-fix! e)))
        (is (= [:debit 5] ((fx "app" 'transact) (debit))))))
    (testing "a match compiled against a sum from before the reload: the same"
      (require (first (nss "types")) :reload)
      (let [e (thrown #(require (first (nss "app")) :reload))]
        (is (= :parsed-types.rule/stale-match (:rule e)))
        (is (= "(require 'parsed-types.stale-fixture.sum 'parsed-types.stale-fixture.app :reload)" (apply-fix! e)))
        (is (= [:debit 5] ((fx "app" 'transact) (debit))))))))

(deftest reloading-the-library-keeps-every-type-test
  (eval-here '(pt/defparsed RlLine [n] (pure ::rl-n-int "n is an integer." [n] (int? n))))
  (eval-here '(pt/defparsed RlOther [n]))
  (eval-here '(pt/defsum RlSum [RlLine RlOther]))
  (eval-here '(defn- rl-match [x] (pt/match RlSum x RlLine l [:line (:n (pt/parsed RlLine l))] RlOther o [:other (:n (pt/parsed RlOther o))])))
  (let [line-type  (ns-resolve 'parsed-types.defparsed-test 'RlLine)
        interfaces #(mapv (fn [n] (Class/forName n false (clojure.lang.DynamicClassLoader.)))
                          ["parsed_types.core.ParsedValue" "parsed_types.core.Sum"])
        before     (interfaces)
        parsed     (eval-here '(pt/parse RlLine {:n 1}))]
    (require 'parsed-types.core :reload)
    (testing "the library's classes are the ones loaded before"
      (is (= before (interfaces))))
    (testing "a value parsed before the reload is still parsed"
      (is (pt/parsed? line-type parsed))
      (is (= {:n 1} (pt/parsed line-type parsed))))
    (testing "a type defined before the reload still parses, and still fails its checks"
      (is (pt/parsed? line-type (pt/parse line-type {:n 2})))
      (is (= ::rl-n-int (:check (try (pt/parse line-type {:n :x}) nil
                                     (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
    (testing "a sum and a match defined before the reload still take the value"
      (is (pt/parsed? (resolve-here 'RlSum) parsed))
      (is (= [:line 1] ((resolve-here 'rl-match) parsed))))
    (testing "a type defined after the reload holds a value parsed before it"
      (eval-here '(pt/defparsed RlEntry [lines]))
      (is (identical? parsed (first (:lines (pt/parsed (resolve-here-class 'RlEntry)
                                                       ((eval-here '(fn [x] (pt/parse RlEntry {:lines [x]}))) parsed)))))))))

(deftest a-hand-written-class-with-the-same-name-is-refused-test
  (eval-here '(pt/defparsed Genuine [n]))
  (let [genuine (ns-resolve 'parsed-types.defparsed-test 'Genuine)
        real    (eval-here '(pt/parse Genuine {:n 1}))]
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
    (testing "and the refused defparsed defined no class: the owner's class is still the loaded one"
      (is (identical? real-type (loaded-class "pt_owner.my_app.bank.Debit")))
      (is (nil? (ns-resolve (the-ns 'pt-owner.my_app.bank) 'Debit)))
      (is (= 5 (sink (eval-in 'pt-owner.my-app.bank '(pt/parse Debit {:amount 5}))))))
    (testing "the owning namespace may redefine it, as a REPL reload does"
      (eval-in 'pt-owner.my-app.bank '(pt/defparsed Debit [amount] (pure ::pos "The amount is positive." [amount] (pos? amount))))
      (let [sink (eval-in 'pt-owner.my-app.bank '(fn [d] (:amount (pt/parsed Debit d))))]
        (is (= 5 (sink (eval-in 'pt-owner.my-app.bank '(pt/parse Debit {:amount 5})))))))))

(deftest defparsed-defines-one-class-test
  (testing "a type is one class: there is no draft class beside it"
    (is (class? (resolve-here-class 'Point)))
    (is (nil? (resolve-here-class 'PointDraft))))
  (testing "so a type may be named like another type plus Draft"
    (eval-in 'pt-roles.a '(pt/defparsed FooDraft [x]))
    (eval-in 'pt-roles.a '(pt/defparsed Foo [y]))
    (is (some? (eval-in 'pt-roles.a '(pt/parse FooDraft {:x 1}))))
    (is (some? (eval-in 'pt-roles.a '(pt/parse Foo {:y 1}))))))

(deftest a-match-compiled-before-a-member-joined-throws-test
  (eval-here '(pt/defparsed MA [x]))
  (eval-here '(pt/defparsed MB [x]))
  (eval-here '(pt/defparsed MC [x]))
  (eval-here '(pt/defsum Grows [MA MB]))
  (eval-here '(defn- grows-match [v] (pt/match Grows v MA a [:a a] MB b [:b b])))
  (eval-here '(pt/defsum Grows [MA MB MC]))
  (let [e (try ((resolve-here 'grows-match) (eval-here '(pt/parse MC {:x 1}))) nil
               (catch clojure.lang.ExceptionInfo e e))]
    (is (= {:type ::pt/not-parsed, :rule :parsed-types.rule/match-compiled-before-member, :handled '[MA MB]}
           (select-keys (ex-data e) [:type :rule :handled])))
    (is (re-find #"compiled before MC joined the sum.*Re-evaluate the match" (ex-message e))))
  (testing "values that are not parsed still get the sink's own error"
    (is (= :parsed-types.rule/got-unparsed
           (:rule (try ((resolve-here 'grows-match) {:x 1}) nil
                       (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

;;; Field names

(pt/defparsed Awkward [count map set str seq into contains? or when keys]
  (pure ::count-int "count is an integer." [count] (int? count)))

(deftest fields-named-like-core-functions-test
  (testing "a field may be named like any clojure.core function or macro"
    (let [d {:count 1, :map 2, :set 3, :str 4, :seq 5, :into 6, :contains? 7, :or 8, :when 9, :keys 10}
          p (pt/parse Awkward d)]
      (is (= {:count 1, :map 2, :set 3, :str 4, :seq 5, :into 6, :contains? 7, :or 8, :when 9, :keys 10} (pt/parsed Awkward p)))
      (is (= p (pt/parse Awkward d)))
      (is (= (hash p) (hash (pt/parse Awkward d))))
      (is (string? (str p)))
      (is (re-find #"#parsed_types.defparsed_test.Awkward\{" (pr-str p)))
      (is (= 20 (:map (pt/parsed Awkward (pt/parse Awkward (assoc d :map 20))))))
      (is (= 9 (let [{:keys [when]} (pt/parsed Awkward p)] when)))
      (is (= ::pt/must-unwrap (:type (thrown #(:count p)))))
      (is (= #{:count :map :set :str :seq :into :contains? :or :when :keys} (set (keys (pt/parsed Awkward p))))))))

(deftest special-form-field-names-test
  (doseq [field '[if do def fn* let* recur throw & new quote]]
    (testing (str field)
      (is (re-find #"special form|simple symbols"
                   (str (compile-error (list 'pt/defparsed 'Special [field]))))))))

(def ^:private order (atom []))

(pt/defparsed Nine [a b c d e f g h i])

(defn- logged [k] (swap! order conj k) k)

(deftest parse-evaluates-a-literal-map-as-clojure-does-test
  (testing "up to 8 keys, in the order written"
    (reset! order [])
    (pt/parse Point {:y (do (swap! order conj :y) 1), :x (do (swap! order conj :x) 1)})
    (is (= [:y :x] @order)))
  (testing "with more, in the order Clojure evaluates the same map literal, which the reader no longer keeps"
    (reset! order [])
    (pt/parse Nine {:i (logged :i) :h (logged :h) :g (logged :g) :f (logged :f) :e (logged :e) :d (logged :d)
                    :c (logged :c) :b (logged :b) :a (logged :a)})
    (let [drafted @order]
      (reset! order [])
      (is (map? {:i (logged :i) :h (logged :h) :g (logged :g) :f (logged :f) :e (logged :e) :d (logged :d)
                 :c (logged :c) :b (logged :b) :a (logged :a)}))
      (is (= @order drafted))
      (is (not= [:i :h :g :f :e :d :c :b :a] drafted) "so not the order written"))))

;;; Equality, and not being a map

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
    (is (not= (Money. 1M 2 nil) {:amount 1M, :count 2}))
    (is (not= {:amount 1M, :count 2} (Money. 1M 2 nil)))
    (is (not (.equals (Money. 1M 2 nil) {:amount 1M, :count 2})))))

(deftest not-a-map-test
  (doseq [[label x] [["a parsed value" (pt/parse Money {:amount 1M, :count 2})]]]
    (testing label
      (is (not (map? x)))
      (is (not (instance? java.util.Map x)) "no java.util.Map interop either")
      (doseq [[op thunk] [["conj" #(conj x [:amount 5M])]
                          ["conj" #(merge x {:amount 5M})]
                          ["empty" #(empty x)]
                          ["with-meta" #(with-meta x {:a 1})]]]
        (is (= {:type ::pt/cannot-change, :op op} (select-keys (thrown thunk) [:type :op])) op))
      (is (thrown? ClassCastException (dissoc x :amount)) "dissoc needs a map"))))

(deftest a-field-cannot-replace-the-checks-test
  (testing "a field named like a declaration, holding one, is only a field: the checks stay those defparsed wrote"
    (eval-here '(pt/defparsed Shadow [Shadow-declaration]
                  (pure ::never "Never passes." [Shadow-declaration] false)))
    (let [parse (eval-here '(fn [v] (pt/parse Shadow {:Shadow-declaration v})))]
      (is (= ::never (:check (try (parse {:parses-to (resolve 'parsed-types.defparsed-test/Shadow), :checks []})
                                  (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))

(deftest field-name-limits-test
  (doseq [[fields re] [['[a.b] #"cannot contain a dot"]
                       ['[a-b a_b] #"compile to the same Java name"]
                       ['[__meta] #"deftype reserves these field names"]
                       ['[parse-proof] #"reserves the field name parse-proof"]
                       ['[parse_proof] #"reserves the field name parse-proof"]]]
    (is (re-find re (str (compile-error (list 'pt/defparsed 'Limits fields)))))))
