(ns parsed-types.readme-test
  "Every clojure block of the README, evaluated in order, against the store in `shop.db`. A form whose line ends in
  `;; => x`, or is followed by that comment, must return `x`. One with `;; throws t` must throw an ex-info of `:type`
  `t`, and when the comment ends in `:failures:`, the next form is the expected `:failures`, and is not evaluated."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn- marker
  "The `;; => ...` or `;; throws ...` comment that ends line `n` of `lines`, or fills line `n + 1`, or nil."
  [lines n]
  (some #(second (re-find #";; ((?:=>|throws) .*)$" (str/trim (str %))))
        [(get lines (dec n)) (when (str/starts-with? (str/trim (str (get lines n))) ";;") (get lines n))]))

(defn- run-block!
  "Evaluate each form of `src` in `*ns*`, checking its marker, and return the namespace it ends in."
  [src]
  (let [lines (str/split-lines src)
        r     (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. src))
        next! #(read {:eof ::eof} r)]
    (loop []
      (let [form (next!)]
        (when-not (= ::eof form)
          (let [m      (marker lines (.getLineNumber r))
                result (try {:value (eval form)} (catch clojure.lang.ExceptionInfo e {:thrown e}))]
            (cond
              (nil? m)
              (is (contains? result :value) (str (pr-str form) " threw " (some-> (:thrown result) ex-message)))

              (str/starts-with? m "=> ")
              (is (= (read-string (subs m 3)) (:value result)) (pr-str form))

              :else
              (let [[_ t failures?] (re-find #"^throws (\S+?),?(?: with these (:failures):)?$" m)]
                (is (= (read-string t) (:type (ex-data (:thrown result)))) (pr-str form))
                (when failures?
                  (is (= (next!) (:failures (ex-data (:thrown result)))) (pr-str form)))))
            (recur)))))
    *ns*))

(deftest every-readme-block-runs-test
  (let [blocks (map second (re-seq #"(?s)```clojure\n(.*?)```" (slurp (io/file "README.md"))))]
    (is (< 5 (count blocks)))
    (binding [*ns* (the-ns 'parsed-types.readme-test)]
      (doseq [[i block] (map-indexed vector blocks)]
        (testing (str "block " i)
          (set! *ns* (run-block! block)))))))
