(ns advent.day8
  (:require [advent.utils :as utils]))

(defrecord Note [unique-signals output])

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

(defn show-signals [signals display]
  (reduce (fn [new-display signal]
            (show-signal signal new-display))
          display
          signals))

(def number-signals-map
  (->> {0 "abcefg"
        1 "cf"
        2 "acdeg"
        3 "acdfg"
        4 "bcdf"
        5 "abdfg"
        6 "abdefg"
        7 "acf"
        8 "abcdefg"
        9 "abcdfg"}
       (map (fn [[num signals-str]]
              [num (as-> signals-str x
                     (clojure.string/split x #"")
                     (set x))]))
       (into {})))

(defn pairwise-diffs [number-signals-map]
  (->> number-signals-map
       (map (fn [[num1 signals1]]
              (map (fn [[num2 signals2]]
                     [(set [num1 num2])
                      (apply clojure.set/difference (sort-by count > [signals1 signals2]))])
                   number-signals-map)))
       (reduce concat [])
       (reduce (fn [acc [pair diff-signals]]
                 (assoc acc pair diff-signals))
               {})))

(def signals-number-map
  (->> number-signals-map
       (map reverse)
       (map #(apply vector %))
       (into {})))

(defn number-to-signals [n]
  (get number-signals-map n))

(defn signals-to-number [signals]
  (->> signals
       set
       (get signals-number-map)))

(defn show-number [n display]
  (->> display
       (show-signals (number-to-signals n))))

(defn print-display [display]
  (->> display
       (map clojure.string/join)
       (clojure.string/join "\n")
       println))

(defn to-note [line]
  (as-> line v
    (clojure.string/split v #"\|")
    (map clojure.string/trim v)
    (map (fn [s]
           (clojure.string/split s #" "))
         v)
    (map (fn [part]
           (map (fn [str]
                  (clojure.string/split str #""))
                part))
         v)
    (Note. (first v)
           (second v))))

(defn to-notes [filename]
  (->> filename
       utils/file-to-seq
       (map to-note)))

(defn signals-number-remap
"Super ugly - takes an original map and a note and converts it into the new number map"
  [prev-signals-number-map note]
 (let [count-to-unique (->> prev-signals-number-map
                            (map (fn [[signals num]]
                                   [#{num} (count signals)]))
                            (map reverse)
                            (map #(apply (partial assoc nil) %))
                            (reduce (partial merge-with clojure.set/union) {}))
       signals-to-possibles (->> note
                                 :unique-signals
                                 (map #(vector (set %)
                                               (->> %
                                                    count
                                                    count-to-unique)))
                                 (into {}))
       signals-to-number (->> signals-to-possibles
                              (filter (fn [[_ numbers]]
                                        (= 1 (count numbers))))
                              (map (fn [[signals numbers]]
                                     [signals (first numbers)]))
                              (into {}))
       number-to-signals (clojure.set/map-invert signals-to-number)
       three (->> signals-to-possibles
                  (filter #(= #{3 2 5} (second %)))
                  (map first)
                  flatten
                  (filter #(= 2
                              (count (clojure.set/intersection %
                                                               (get number-to-signals 1)))))
                  first)
       five (->> signals-to-possibles
                 (filter #((second %) 5))
                 (map first)
                 flatten
                 (filter #(not= three %))
                 (filter #(= 3
                             (count (clojure.set/intersection % (get number-to-signals 4)))))
                 first)
       two (->> signals-to-possibles
                (filter #((second %) 2))
                (map first)
                flatten
                (filter #(not= five %))
                (filter #(not= three %))
                first)
       six (->> signals-to-possibles
                (filter #((second %) 6))
                (map first)
                (filter #(= 1 (count (clojure.set/intersection % (get number-to-signals 1)))))
                first)
       zero (->> signals-to-possibles
                 (filter #((second %) 0))
                 (map first)
                 flatten
                 (filter #(not= six %))
                 (filter #(= 4 (count (clojure.set/intersection % three))))
                 first)
       nine (->> signals-to-possibles
                 (filter #((second %) 9))
                 (map first)
                 flatten
                 (filter #(not= six %))
                 (filter #(not= zero %))
                 first)]
   (merge signals-to-number
          {nine 9
           six 6
           three 3
           five 5
           zero 0
           two 2})))

(comment
  (clojure.pprint/pprint
   (signals-number-remap
    signals-number-map
    (->> "input/day8test.txt"
         utils/file-to-seq
         first
         to-note)))
  )

(comment
;; part 2
  (->> "input/day8.txt"
       utils/file-to-seq
       (map to-note)
       (reduce (fn [acc note]
                 (let [mapping (signals-number-remap signals-number-map note)
                       output (->> note
                                   :output
                                   (map set)
                                   (map mapping)
                                   reverse
                                   (map-indexed (fn [i num]
                                                  (* num
                                                     (apply * (repeat i 10)))))
                                   (reduce +))]
                   (+ output acc)))
               0))

;; part 1
  (let [uniques-count (->> number-signals-map
                           (map (fn [[num signals]]
                                  [#{num} (count signals)]))
                           (map reverse)
                           (map #(apply (partial assoc nil) %))
                           (reduce (partial merge-with clojure.set/union) {})
                           (filter (fn [[_ numbers]]
                                     (= 1 (count numbers))))
                           (map first)
                           set)] ;; they give this to you already but whatever
    (->> "input/day8.txt"
         utils/file-to-seq
         (reduce (fn [total newline]
                   (+ total
                      (->> newline
                           to-note
                           :output
                           (map count)
                           (map uniques-count)
                           (filter (complement nil?))
                           count)))
                 0)))

    ;; for more fun
  (as-> [1 3 3 7] v
    (map number-to-signals v)
    (map #(show-signals % display) v)
    (map print-display v))

;; for fun
  (->> "input/day8test.txt"
       to-notes
       (map :output)
       first
       (map (fn [signals]
              (print-display (show-signals signals display))
              (println))))
  )