(ns advent.main)

(comment
;; day 1 1
  (let [reader (clojure.java.io/reader "input/day1_1.txt")
        comp-nums (map #(Integer/parseInt %) (line-seq reader))
        nums (drop 1 comp-nums)
        to-compare (partition 2 
                              (interleave nums comp-nums))
        increases (filter (partial apply >) to-compare)]
    (count increases))) 

(comment
;; day 1 2
  (let [reader (clojure.java.io/reader "input/day1_1.txt")
        comp-a (map #(Integer/parseInt %) (line-seq reader))
        comp-b (drop 1 comp-a)
        comp-c (drop 1 comp-b)
        comp-d (drop 1 comp-c)
        nums (drop 1 comp-nums)
        to-compare (partition 2
                              (interleave nums comp-nums))
        increases (filter (partial apply >) to-compare)]
    (count increases))) 