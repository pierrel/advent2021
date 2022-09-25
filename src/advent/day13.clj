(ns advent.day13
  (:require [advent.utils :as utils]))

(defn input-to-dots [filename]
  (->> filename
       utils/file-to-seq
       (filter #(re-matches #"\d+,\d+" %))
       (map #(clojure.string/split % #","))
       (map #(map utils/parseint %))))

(comment
  (input-to-field "input/day13test.txt"))

(defn in-coord-lookup? [lookup dot]
  (if-let [ys (get lookup (first dot))]
    (contains? ys (last dot))
    false))

(comment
  (let [lookup {3 #{4 5}
                8 #{6}}]
    [(in-coord-lookup? lookup [2 3])
     (in-coord-lookup? lookup [3 5])
     (in-coord-lookup? lookup [3 1])]))

(defn dots-to-lookup [dots]
  (->> dots
       (map (fn [[x y]]
              {x #{y}}))
       (apply (partial merge-with clojure.set/union))))

(defn print-dots [dots]
  (let [xs (map first dots)
        ys (map last dots)
        max-x (apply max xs)
        max-y (apply max ys)
        lookup (dots-to-lookup dots)
        field (repeat (inc max-y) (repeat (inc max-x) 0))]
    (println "--------")
    (->> field
         (utils/map-indexed-2d (fn [dot _]
                                 (if (in-coord-lookup? lookup dot)
                                   "#"
                                   ".")))
         (map clojure.string/join)
         (clojure.string/join "\n")
         println)
    (println "--------")))


(comment
  (vector 1 0)
  (->> "input/day13test.txt"
       input-to-dots
       print-dots))

(defn input-to-instructions
  "Takes an input file and returns the instructions as
  [index value]"
  [filename]
  (->> filename
       utils/file-to-seq
       (filter #(re-matches #"fold along.*" %))
       (map #(re-matches #".*(x|y)=(\d+)" %))
       (map (fn [[_ coord val]]
              [(case coord
                 "x" 0
                 "y" 1)
               (utils/parseint val)]))))
(comment
  (input-to-instructions "input/day13test.txt"))

(defn fold [[index fold-line] dot]
  (map-indexed (fn [i e]
                 (if (and (= i index)
                          (< fold-line e))
                   (- e
                      (* 2 (- e fold-line)))
                   e))
               dot))

(comment
  (fold [0 4] [10 0]))

(defn count-dots [dots]
  (count (set dots)))

(comment
  (count-dots [[0 10]
               [9 5]
               [5 5]
               [9 5]]))

(comment
  (let [file "input/day13.txt"
        dots (input-to-dots file)
        instructions (input-to-instructions file)]
    (print-dots (loop [latest-dots dots
                       instructions-left instructions]
                  (println (count-dots latest-dots))
                  (if (empty? instructions-left)
                    latest-dots
                    (recur (map (partial fold (first instructions-left))
                                latest-dots)
                           (rest instructions-left)))))))
