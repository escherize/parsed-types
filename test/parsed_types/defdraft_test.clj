(ns parsed-types.defdraft-test
  "What `defdraft` refuses at compile time, and the declaration it writes."
  (:require
   [clojure.test :refer [deftest is testing]]
   [parsed-types.core :as pt]))

(set! *warn-on-reflection* true)

(defn- compile-error
  "The message of the error compiling `form` in this namespace, or nil when it compiles."
  [form]
  (try (binding [*ns* (the-ns 'parsed-types.defdraft-test)] (eval form)) nil
       (catch Throwable e
         (ex-message (or (ex-cause e) e)))))

(deftest compile-errors-test
  (doseq [[label form re]
          [["no arrow"
            '(pt/defdraft ADraft => A [a])
            #"write \(defdraft DraftName -> ParsedName"]
           ["a check that reads a field the draft does not have"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" [b] true))
            #"reads fields the draft does not have: \[b\]"]
           ["a check that uses a field it does not name"
            '(pt/defdraft ADraft -> A [a b] (pure ::c "d" [a] (< a b)))
            #"uses fields its argument vector does not name: \[b\]"]
           ["a check that uses a field named like a clojure.core fn, without naming it"
            '(pt/defdraft ADraft -> A [a name] (pure ::c "d" [a] (string? (name a))))
            #"uses fields its argument vector does not name: \[name\]"]
           ["a check that names a field twice"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" [a a] true))
            #"reads a field twice"]
           ["an unqualified check name"
            '(pt/defdraft ADraft -> A [a] (pure :c "d" [a] true))
            #"qualified keyword"]
           ["no doc"
            '(pt/defdraft ADraft -> A [a] (pure ::c [a] true))
            #"needs a doc string"]
           ["no body"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" [a]))
            #"needs a body"]
           ["a fetch that is not declared"
            '(pt/defdraft ADraft -> A [a] (external ::c "d" [a] [x ::nope] true))
            #"pairs of a local and a fetch declared"]
           ["a fetch over a field the draft does not have"
            '(pt/defdraft ADraft -> A [a] (fetch ::f [b] identity))
            #"fields of the draft"]
           ["a child field that is not a field"
            '(pt/defdraft ADraft -> A [a] (children b A))
            #"child field must be a field"]
           ["a name used twice"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" [a] true) (pure ::c "d" [a] true))
            #"used twice"]
           ["a field named twice"
            '(pt/defdraft ADraft -> A [a a])
            #"field is named twice"]
           ["a field declared as a child twice"
            '(pt/defdraft ADraft -> A [a] (child a A) (children a A))
            #"declared as a child twice"]
           ["a fetch bound to a field's name"
            '(pt/defdraft ADraft -> A [a] (external ::c "d" [] [a ::f] true) (fetch ::f [a] identity))
            #"fetch is bound to a field's name: \[a\]"]
           ["a pure check that binds a fetch"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" [a] [r ::f] true) (fetch ::f [a] identity))
            #"a pure check cannot fetch"]
           ["an unknown option"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" {:status-code 422} [a] true))
            #"unknown options \[:status-code\]"]
           ["a status that is not an integer"
            '(pt/defdraft ADraft -> A [a] (pure ::c "d" {:status "teapot"} [a] true))
            #":status must be an integer"]
           ["an unknown clause"
            '(pt/defdraft ADraft -> A [a] (check ::c "d" [a] true))
            #"a clause is \(pure"]]]
    (testing label
      (is (re-find re (str (compile-error form)))))))

(pt/defdraft PointDraft -> Point [x y]
  (pure ::x-int "x is an integer." [x] (int? x))
  (pure ::y-int "y is an integer." {:status 422} [y] (int? y))
  (pure ::on-diagonal "x equals y." [x y] (= x y))
  (external ::known "The point is known." [] [row ::row] (some? row))
  (fetch ::row [x] (fn [tuples] (into {} (map (fn [t] [t :row])) tuples))))

(deftest declaration-test
  (testing "defdraft writes a plain declaration: the data you would write by hand"
    (let [decl (pt/declaration (->PointDraft 1 1))]
      (is (= Point (:parses-to decl)))
      (is (= [{:name ::x-int, :doc "x is an integer.", :kind :pure, :reads [:x]}
              {:name ::y-int, :doc "y is an integer.", :kind :pure, :reads [:y], :status-code 422}
              {:name ::on-diagonal, :doc "x equals y.", :kind :pure, :reads [:x :y]}
              {:name ::known, :doc "The point is known.", :kind :external, :reads [], :fetches [::row]}]
             (mapv #(dissoc % :f) (:checks decl))))
      (is (= [{:name ::row, :reads [:x]}] (mapv #(dissoc % :f) (:fetches decl))))))
  (testing "and parses with it"
    (is (= {:x 2, :y 2} (pt/parse (->PointDraft 2 2))))
    (is (= 422 (:status-code (try (pt/parse (->PointDraft 1 :y)) (catch clojure.lang.ExceptionInfo e (ex-data e))))))))
