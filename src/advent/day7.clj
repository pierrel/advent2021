(ns advent.day7
  (:require [advent.utils :as utils]))

(defn diff [& nums]
  (apply - 
         (sort > nums)))

(defn diff-exp [from to]
  (reduce +
          (range (inc (diff from to)))))

(defn group-count [map e]
  (merge-with +
              map
              {e 1}))

(defn grouped [seq]
  (reduce group-count
          {}
          seq))

(comment
;; part 2
  (let [input (->> (utils/file-to-seq "input/day7.txt")
                   first
                   utils/comma-sep
                   (map #(Integer/parseInt %))
                   grouped)
        possible-tos (range 0
                            (inc (apply max (keys input))))]
    (reduce (fn [lowest new-to]
              (->> input
                   (map (fn [[from count]]
                          (* (diff-exp from new-to) count))) ;; only diff from part 1
                   (reduce +)
                   (min lowest)))
            Integer/MAX_VALUE
            possible-tos))
  ;; part 1
  (let [input (->> (utils/file-to-seq "input/day7.txt")
                   first
                   utils/comma-sep
                   (map #(Integer/parseInt %))
                   grouped)
        possible-tos (range 0
                            (inc (apply max (keys input))))]
    (reduce (fn [lowest new-to]
              (->> input
                   (map (fn [[from count]]
                          (* (diff from new-to) count)))
                   (reduce +)
                   (min lowest)))
            Integer/MAX_VALUE
            possible-tos))

;; part 1 test input
  (let [input (->> (utils/file-to-seq "input/day7test.txt")
                   first
                   utils/comma-sep
                   (map #(Integer/parseInt %)))
        possible-tos (range 0 (inc (apply max input)))]
    (->> possible-tos
         (map (fn [to]
                (map (fn [from]
                       (diff from to))
                     input)))
         (map (partial reduce +))
         (map-indexed vector)
         (sort-by second)
         first
         second))
  )