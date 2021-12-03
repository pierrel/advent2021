(ns advent.day3
  (:require [advent.utils :as utils]
            [clojure.string :as string]))

(defn vector-bits [str-bits]
  (string/split str-bits #""))

(defn inc-occurrences [occurrences new-occ]
  (let [current-occ (get occurrences new-occ)
        new-val (if (nil? current-occ)
                  1
                  (+ 1 current-occ))]
    (assoc occurrences new-occ new-val)))

(defn occurrences-in [seq]
  (reduce inc-occurrences
          {}
          seq))

(defn key-of-opt-val
 "Returns the key of the optimal value wrt opt-fun of map m" 
  [m opt-fun]
  (key (apply opt-fun val m)))

(comment
  (let [diags (map vector-bits (utils/file-to-seq "input/day3.txt"))
        cols (partition (count diags)
                        (apply interleave diags))
        col-occurrences (map occurrences-in cols)
        maxs-mins [(map #(key (apply max-key val %)) col-occurrences) ;; there's a better way
                   (map #(key (apply min-key val %)) col-occurrences)]]
    (apply * (map #(Integer/parseInt (string/join %) 2) maxs-mins))))