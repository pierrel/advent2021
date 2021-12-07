(ns advent.days5
  (:require [advent.utils :as utils]))

(defrecord Point [x y])

(def plane
  (repeatedly #(repeat 0)))

(defn point-from-str [s]
  (let [int-points (->> (clojure.string/split s #",")
                        (map #(Integer/parseInt %)))]
    (Point. (first int-points)
            (second int-points))))

(defn line-from-str [s]
  (->> (clojure.string/split s #"->")
       (map clojure.string/trim)
       (map point-from-str)))

(defn limit-plane [x y plane]
  (map (partial take x)
       (take y plane)))

(defn walk [from to]
  (let [{x1 :x y1 :y} from
        {x2 :x y2 :y} to]
    (cond
      (= x1 x2) (let [[y11 y22] (sort < [y1 y2])]
                  (map #(Point. x1 %)
                       (range y11 (inc y22))))
      (= y1 y2) (let [[x11 x22] (sort < [x1 x2])]
                  (map #(Point. % y1)
                       (range x11 (inc x22))))
      :else '())))

(defn walk-all [from to]
  (let [w (walk from to)
        {x1 :x y1 :y} from
        {x2 :x y2 :y} to]
    (if (empty? w)
      (let [ydir (if (> y1 y2)
                   dec
                   inc)
            xdir (if (> x1 x2)
                   dec
                   inc)]
        (loop [cur from
               path (list from)]
          (if (= cur to)
            path
            (let [next (Point. (xdir (:x cur))
                               (ydir (:y cur)))]
              (recur next
                     (conj path next))))))
      w)))

(defn inc-plane [plane point]
  (map-indexed (fn [y line]
                 (map-indexed (fn [x elem]
                                (if (and (= x (:x point))
                                         (= y (:y point)))
                                  (inc elem)
                                  elem))
                              line))
               plane))

(defn point-code [point]
  (format "%s,%s" 
          (-> point :x str)
          (-> point :y str)))

(defn inc-point-occurrences [occ point]
  (update occ
          (point-code point)
          (fn [old]
            (if (nil? old)
              1
              (inc old)))))

(comment
;; part 2
  (let [res (->> (utils/file-to-seq "input/day5.txt")
                 (map line-from-str)
                 (map (partial apply walk-all))
                 flatten
                 (reduce inc-point-occurrences {})
                 vals
                 (filter (partial <= 2))
                 count)]
    res)

;; part 1
  (let [res (->> (utils/file-to-seq "input/day5.txt")
                 (map line-from-str)
                 (map (partial apply walk))
                 flatten
                 (reduce inc-point-occurrences {})
                 vals
                 (filter (partial <= 2))
                 count)]
    res)

;; For fun
  (clojure.pprint/pprint
   (->> (utils/file-to-seq "input/day5test.txt")
        (map line-from-str)
        (map (partial apply walk))
        flatten
        (reduce inc-plane plane)
        (limit-plane 10 10))))
