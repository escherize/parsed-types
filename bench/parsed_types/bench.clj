(ns parsed-types.bench
  "Rough timings of the library's hot paths, outside the test suite: run with `clojure -M:bench`. Each row is the
  mean time of one call on one thread, over 2 s after a 2 s warm-up. Expect a few percent of noise between runs.

  Baselines on an Apple M4 Max, JDK 25, before (ce9b57a) and after the walk's leaf fast path and the single
  type check in `pt/parsed`:

  | Case                                                | ce9b57a | now     |
  |-----------------------------------------------------|---------|---------|
  | parse, 3 fields                                     | 2.9 us  | 1.1 us  |
  | parse, 20 fields                                    | 5.7 us  | 2.2 us  |
  | parse, a field of 100 maps of 9 keys                | 277 us  | 36 us   |
  | parse, a field of 1k values nested 2 deep           | 116 us  | 25 us   |
  | parse-all 1k lines with a fetch, then their holder  | 3.3 ms  | 1.3 ms  |
  | parse-all of 10k 3-field maps                       | 35 ms   | 10 ms   |
  | pt/parsed on a type                                 | 104 ns  | 66 ns   |
  | pt/parsed on a sum                                  | 468 ns  | 258 ns  |"
  (:require
   [parsed-types.core :as pt]))

(defn- rows-by-id [tuples] (into {} (map (fn [[id :as t]] [t {:id id}])) tuples))

(pt/defparsed Three [a b c]
  (pure ::a-int "a is an integer." [a] (int? a))
  (pure ::b-string "b is a string." [b] (string? b))
  (pure ::c-pos "c is positive." [c] (pos? c)))

(pt/defparsed Twenty [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18 f19]
  (pure ::f0-int "f0 is an integer." [f0] (int? f0))
  (pure ::f19-int "f19 is an integer." [f19] (int? f19)))

(pt/defparsed Blob [v])

(pt/defparsed Line [id qty]
  (pure ::qty-pos "qty is positive." [qty] (pos-int? qty))
  (external ::known "The row exists." [] [row ::row] (some? row))
  (fetch ::row [id] rows-by-id))

(pt/defparsed Holder [lines])

(pt/defparsed Other [x])

(pt/defsum Either [Three Other])

(defn- mean-ns
  "The mean time of one call of `f`, in nanoseconds: run for about `ms` after a warm-up of the same length."
  [ms f]
  (let [run (fn []
              (let [deadline (+ (System/nanoTime) (* ms 1000000))]
                (loop [n 0]
                  (if (< (System/nanoTime) deadline)
                    (do (f) (recur (inc n)))
                    n))))
        _   (run)
        t0  (System/nanoTime)
        n   (run)]
    (/ (double (- (System/nanoTime) t0)) n)))

(defn- shown [ns]
  (cond (< ns 1e3) (format "%.0f ns" ns)
        (< ns 1e6) (format "%.1f us" (/ ns 1e3))
        :else      (format "%.1f ms" (/ ns 1e6))))

(defn -main [& _]
  (let [payload (vec (for [i (range 100)] (zipmap [:a :b :c :d :e :f :g :h :i] (repeat i))))
        nested  (vec (for [i (range 100)] (vec (range i (+ i 10)))))
        line    (fn [i] {:id (mod i 50), :qty 1})
        three   (pt/parse Three {:a 1, :b "b", :c 1})
        cases   [["parse, 3 fields"
                  #(pt/parse Three {:a 1, :b "b", :c 1})]
                 ["parse, 20 fields"
                  #(pt/parse Twenty {:f0 0, :f1 1, :f2 2, :f3 3, :f4 4, :f5 5, :f6 6, :f7 7, :f8 8, :f9 9
                                               :f10 10, :f11 11, :f12 12, :f13 13, :f14 14, :f15 15, :f16 16
                                               :f17 17, :f18 18, :f19 19})]
                 ["parse, a field of 100 maps of 9 keys"
                  #(pt/parse Blob {:v payload})]
                 ["parse, a field of 1k values nested 2 deep"
                  #(pt/parse Blob {:v nested})]
                 ["parse-all 1k lines with a fetch, then their holder"
                  #(pt/parse Holder {:lines (pt/parse-all Line (mapv line (range 1000)))})]
                 ["parse-all of 10k 3-field maps"
                  (let [ms (vec (repeat 10000 {:a 1, :b "b", :c 1}))] #(pt/parse-all Three ms))]
                 ["pt/parsed on a type"
                  #(pt/parsed Three three)]
                 ["pt/parsed on a sum"
                  #(pt/parsed Either three)]]]
    (println "| Case | Time |")
    (println "|---|---|")
    (doseq [[label f] cases]
      (println (str "| " label " | " (shown (mean-ns 2000 f)) " |")))))
