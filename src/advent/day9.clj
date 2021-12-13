(ns advent.day9
  (:require [advent.utils :as utils]))


(defn seq-around 
"Given 3 seqs returns a lazy seq of the middles with each of the adjacent elements
 in order of up, left, down, right.
 [1 2 3]
 [3 2 1]
 [5 6 7]
 => [[3 [1 5 nil 2]] [2 [2 6 3 1]] ...]"
  [top middle bottom]
  (map-indexed (fn [idx elem]
                 [elem [(nth top idx nil)
                        (nth bottom idx nil)
                        (nth middle (dec idx) nil)
                        (nth middle (inc idx) nil)]])
               middle))

(comment
  (= [[3 [1 5 nil 2]]
      [2 [2 6 3 1]]
      [1 [3 7 2 nil]]]
     (seq-around [1 2 3] 
                 [3 2 1] 
                 [5 6 7])))

  (= (seq-around []
                 [1 2 3]
                 [3 2 1])
     [[1 [nil 3 nil 2]]
      [2 [nil 2 1 3]]
      [3 [nil 1 2 nil]]])

(defn three-seq-groups
  "Returns a list of three-line groups."
  [orig]
  (map (fn [idx]
         [(nth orig (dec idx) [])
          (nth orig idx [])
          (nth orig (inc idx) [])])
       (-> orig count range)))

(defn is-smaller?
  "returns true if `elem` is smaller than all elems in `than-coll`"
  [elem than-coll]
  (every? #(< elem %)
          than-coll))


(comment
;; part 1
  (->> "input/day9.txt"
       utils/file-to-seq
       (map #(clojure.string/split % #""))
       (map (fn [str-seq] (map #(Integer/parseInt %) str-seq)))
       three-seq-groups
       (map #(apply seq-around %))
       (reduce concat [])
       (reduce (fn [local-smallest [elem surrounding-elems]]
                 (if (is-smaller? elem 
                                  (filter (complement nil?) surrounding-elems))
                   (conj local-smallest elem)
                   local-smallest))
               [])
       (map inc)
       (apply +))
  
  ;; testing
  (->> [[1 2 3 4]
        [4 3 2 1]
        [5 6 7 8]
        [8 7 6 5]]
       three-seq-groups
       (map #(apply seq-around %))
       (reduce concat [])
       (reduce (fn [local-smallest [elem surrounding-elems]]
                 (if (is-smaller? elem 
                                  (filter (complement nil?) surrounding-elems))
                   (conj local-smallest elem)
                   local-smallest))
               []))
  )