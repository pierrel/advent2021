(ns advent.day1)

(defn num-seq
  "Returns a lazy sequence of integers from `filename`,
   a file of newline-separated numbers."
  [filename]
  (let [reader (clojure.java.io/reader filename)]
    (map #(Integer/parseInt %)
         (line-seq reader))))

(defn increases
  "Returns the number of ordered, pairwise increases between numbers in `nums`"
  [nums]
  (let [main-nums (drop 1 nums)
        to-compare (partition 2
                              (interleave main-nums nums))
        increases (filter (partial apply >) to-compare)]
    (count increases)))

(comment
  ;; part 1
  (increases (num-seq "input/day1_1.txt"))

  ;; part 2
  (let [nums (num-seq "input/day1_1.txt")
        starts (map #(drop % nums)
                    (range 3))
        triples (partition 3
                           (apply interleave starts))
        triple-sums (map (partial apply +) triples)]
    (increases triple-sums)))

   