(ns advent.day6
  (:require [advent.utils :as utils]
            [clojure.string :as s]))

(defn advance [lanternfish]
  (if (= 0 lanternfish)
    [6 8]
    (dec lanternfish)))

(defn counter [map check]
  (inc (get map check 0)))

(defn grouped [lanternfish]
  (reduce (fn [acc cur]
            (assoc acc
                   cur
                   (counter acc cur)))
          {}
          lanternfish))

(defn advance-map [fish-count]
  (let [before-inc (->> fish-count
                        (map (fn [[fish count]]
                               [(advance fish) count]))
                        (into {}))]
    (reduce (fn [acc [fish count]]
          (merge-with +
                      acc
                      (if (vector? fish)
                        (into {}
                              (map #(vector % count)
                                   fish))
                        {fish count})))
            {}
            before-inc)))

(comment
;; part 2
  (as-> (utils/file-to-seq "input/day6.txt") v
    (first v)
    (s/split v #",")
    (map #(Integer/parseInt %) v)
    (grouped v)
    (iterate advance-map v)
    (nth v 256)
    (vals v)
    (reduce + v))
  
;; part 1
  (as-> (utils/file-to-seq "input/day6.txt") v
    (first v)
    (s/split v #",")
    (map #(Integer/parseInt %) v)
    (iterate #(->> %
                   (map advance)
                   flatten)
             v)
    (nth v 80)
    (count v)))
