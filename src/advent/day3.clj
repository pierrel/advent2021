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

(defn bin-string-seq-to-int [s]
  (Integer/parseInt (string/join s) 2))

(defn keys-of-optim-vals [optim-fn occurrences]
  (let [optim-val (apply optim-fn (vals occurrences))]
    (map first
         (filter (fn [[_ v]]
                   (= v optim-val))
                 occurrences))))

(defn choose-optm [occurrences-map optim-fn default]
  (let [keys (keys-of-optim-vals optim-fn occurrences-map)]
    (if (= 1 (count keys))
      (first keys)
      default)))

(comment
  ;; Part 2
  (let [diags (->> (utils/file-to-seq "input/day3.txt")
                   (map vector-bits))
        final-pos (count (first diags))
        readings (map (fn [[comp default]]
                        (loop [remaining diags
                               pos 0]
                          (if (or (=  1 (count remaining))
                                  (= pos final-pos))
                            (first remaining)
                            (let [slice          (map #(nth % pos)
                                                      remaining)
                                  occurrences    (occurrences-in slice)
                                  optm-occurrence (choose-optm occurrences
                                                               comp
                                                               default)]
                              (recur (filter #(= optm-occurrence (nth % pos))
                                             remaining)
                                     (inc pos))))))
                      [[max "1"]
                       [min "0"]])
        readings-ints (map bin-string-seq-to-int
                           readings)]
    (apply * readings-ints))

   ;; Part 1
  (let [diags (map vector-bits (utils/file-to-seq "input/day3.txt"))]
    (apply * (map bin-string-seq-to-int
                  (let [cols (partition (count diags)
                                        (apply interleave diags))
                        col-occurrences (map occurrences-in cols)]
                    (map (fn [optim-fn]
                           (map (fn [occurrences]
                                  (first (keys-of-optim-vals optim-fn
                                                             occurrences)))
                                col-occurrences))
                         [max min]))))))
