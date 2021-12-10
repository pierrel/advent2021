(ns advent.day8
  (:require [advent.utils :as utils]))

(defrecord Note [signal output])

(def display
  (repeat 7
          (repeat 6 ".")))

(defn positions [signal]
  (case signal
    "a" [[0 1]
         [0 2]
         [0 3]
         [0 4]]
    "b" [[1 0]
         [2 0]]
    "c" [[1 5]
         [2 5]]
    "d" [[3 1]
         [3 2]
         [3 3]
         [3 4]]
    "e" [[4 0]
         [5 0]]
    "f" [[4 5]
         [5 5]]
    "g" [[6 1]
         [6 2]
         [6 3]
         [6 4]]))

(defn show-signal [signal display]
  (let [on-positions (as-> signal v
                       (positions v)
                       (interleave v (repeat true))
                       (partition 2 v)
                       (map #(apply vector %) v)
                       (into {} v))]
    (map-indexed (fn [y row]
                   (map-indexed (fn [x i]
                                  (if (get on-positions [y x])
                                    "0"
                                    i))
                                row))
                 display)))

(show-signal "a" display)

(defn show-signals [signals display]
  (reduce (fn [new-display signal]
            (show-signal signal new-display))
          display
          signals))

(defn number-to-signals [n]
  (as-> (case n
         0 "abcefg"
         1 "cf"
         2 "acdeg"
         3 "acdfg"
         4 "bcdf"
         5 "abdfg"
         6 "abdefg"
         7 "acf"
         8 "abcdefg"
         9 "abcdfg") s
       (clojure.string/split s #"")))


(defn show-number [n display]
  (->> display
       (show-signals (number-to-signals n))))

(defn print-display [display]
  (->> display
       (map clojure.string/join)
       (clojure.string/join "\n")
       println))

(defn to-notes [filename]
  (->> filename
       utils/file-to-seq
       (map #(as-> % v
               (clojure.string/split v #"\|")
               (map clojure.string/trim v)
               (map (fn [s]
                      (clojure.string/split s #" "))
                    v)
               (map (fn [part]
                      (map (fn [str]
                             (clojure.string/split str #""))
                           part))
                    v)))
       (map #(Note. (first %) (last %)))))

(comment
;; for fun
  (->> "input/day8test.txt"
       to-notes
       (map :output)
       first
       (map (fn [signals]
              (print-display (show-signals signals display))
              (println)))))

