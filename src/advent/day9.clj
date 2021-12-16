(ns advent.day9
  (:require [advent.utils :as utils]))

(defrecord Point [x y])
(defrecord ElementPoint [element point])

(defn points-around [point]
  (let [{:keys [x y]} point]
    [(Point. x (dec y))
     (Point. x (inc y))
     (Point. (dec x) y)
     (Point. (inc x) y)]))

(defn elem-at [point seq2d]
  (nth (nth seq2d (:y point) '())
       (:x point) nil))

(defn elem-points-around [point seq2d]
  (->> point
       points-around
       (map (fn [point]
              (ElementPoint. (elem-at point seq2d)
                             point)))))

(defn is-smaller?
  "returns true if `elem` is smaller than all elems in `than-coll`"
  [elem than-coll]
  (every? #(< elem %)
          than-coll))

(defn in-basin? [basin-elem-pt elem-pt]
  (let [elem (:element elem-pt)]
    (and (not (nil? elem))
         (not= 9 elem)
         (< (:element basin-elem-pt)
            elem))))

(defn surrounding-basin [sink seq2d]
  (filter (partial in-basin? sink)
          (elem-points-around (:point sink) seq2d)))

(defn basin-in [sink seq2d]
  (loop [basins #{}
         to-check #{sink}]
    (if (empty? to-check)
      basins
      (let [surrounding-check (->> to-check
                                   (map #(surrounding-basin % seq2d))
                                   flatten
                                   set)
            surrounding-unchecked (clojure.set/difference surrounding-check
                                                          basins)]
        (recur (clojure.set/union basins to-check)
               surrounding-unchecked)))))

(comment
;; part 2
  (let [input (->> "input/day9.txt"
                   utils/file-to-seq
                   (map #(clojure.string/split % #""))
                   (map (fn [str-seq]
                          (map #(Integer/parseInt %) str-seq))))
        elems-around-ptd (map-indexed
                          (fn [y-idx seq]
                            (map-indexed
                             (fn [x-idx elem]
                               (let [point (Point. x-idx y-idx)]
                                 [(ElementPoint. elem point)
                                  (map #(elem-at % input) (points-around point))]))
                             seq))
                          input)
        basins (->> elems-around-ptd
                    (reduce concat)
                    (reduce (fn [sinks [element-point elements-around]]
                              (if (is-smaller? (:element element-point)
                                               (filter (complement nil?) elements-around))
                                (conj sinks element-point)
                                sinks))
                            [])
                    (map (fn [sink]
                           (basin-in sink input))))]
    (->> basins
         (map count)
         (sort >)
         (take 3)
         (reduce * 1)))


;; part 1
  (let [input (->> "input/day9.txt"
                   utils/file-to-seq
                   (map #(clojure.string/split % #""))
                   (map (fn [str-seq]
                          (map #(Integer/parseInt %) str-seq))))
        elems-and-around (map-indexed
                          (fn [y-idx seq]
                            (map-indexed
                             (fn [x-idx elem]
                               [elem (->> (Point. x-idx y-idx)
                                          points-around
                                          (map #(elem-at % input))
                                          (filter (complement nil?)))])
                             seq))
                          input)]
    (->> elems-and-around
         (reduce concat [])
         (reduce (fn [sinks elem-and-around]
                   (if (apply is-smaller? elem-and-around)
                     (conj sinks (first elem-and-around))
                     sinks))
                 [])
         (map inc)
         (reduce + 0))))