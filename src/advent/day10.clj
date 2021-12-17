(ns advent.day10
  (:require [advent.utils :as utils]))

(def close-points
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(def pairs
  {"(" ")"
   "[" "]"
   "{" "}"
   "<" ">"})

(defn first-error 
"Returns the first error character or nil line is unbalanced"
  [line]
  (loop [chunk-starts '()
           rem line]
      (if (empty? rem)
        nil
        (let [cur (first rem)
              most-recent-opening (first chunk-starts)
              is-opening (get pairs cur)]
          (if is-opening
            (recur (conj chunk-starts cur)
                   (rest rem))
            (if (= cur (get pairs most-recent-opening))
              (recur (rest chunk-starts)
                     (rest rem))
              cur))))))

(comment
  (->> "input/day10.txt"
       utils/file-to-seq
       (map (fn [line]
              (clojure.string/split line #"")))
       (map first-error)
       (filter (complement nil?))
       (map close-points)
       (reduce +)))