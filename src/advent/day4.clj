(ns advent.day4
  (:require [clojure.string :as string]
            [advent.utils :as utils]))

(defrecord Bingo [draws boards])
(defrecord Location [elem marked?])

(defn str-to-row [str]
  (map #(Location. % false)
       (string/split (string/trim str) #"\s+")))

(defn file-to-bingo [filename]
  (loop [rem-lines (utils/file-to-seq filename)
         draws nil
         boards []
         cur-board []]
    (let [cur-line (first rem-lines)
          new-rem-lines (rest rem-lines)]
      (cond
        (nil? cur-line) (Bingo. draws (conj boards
                                            cur-board))
        (nil? draws)  (recur new-rem-lines
                             (string/split cur-line #",")
                             boards
                             cur-board)
        (string/blank? cur-line)  (recur new-rem-lines
                                         draws
                                         (if (empty? cur-board)
                                           boards
                                           (conj boards cur-board))
                                         [])
        :else  (recur new-rem-lines
                      draws
                      boards
                      (conj cur-board (str-to-row cur-line)))))))

(defn print-board [board]
  (map (fn [row] (map #(format "%s %s" (:elem %) (:marked? %))
                      row))
       board))

(defn draw-on-board [elem board]
  (map (fn [row]
         (map #(assoc % :marked?
                      (or (= elem (:elem %))
                          (:marked? %)))
              row))
       board))

(defn draws-on-board [elems board]
  (reduce (fn [cur-board elem]
            (draw-on-board elem cur-board))
          board
          elems))

(defn win? [board]
  (some true?
        (map #(some true?
                    (map (partial every? :marked?)  %))
             [board
              (partition (-> board first count)
                         (apply interleave board))])))

(defn draw [{:keys [draws boards]}]
  (Bingo. (rest draws)
          (map (partial draw-on-board (first draws))
               boards)))

(comment

;; part 1
  (let [bingo (file-to-bingo "input/day4.txt")
        [last-draw final-bingo] (loop [cur-bingo bingo
                                       last-draw (-> bingo :draws first)]
                                  (if (some win? (:boards cur-bingo))
                                    [last-draw cur-bingo]
                                    (recur (draw cur-bingo)
                                           (-> cur-bingo :draws first))))
        winning-board (first (filter win? (:boards final-bingo)))]
    (* (Integer/parseInt last-draw)
       (apply + (->> (flatten winning-board)
                     (filter (complement :marked?))
                     (map :elem)
                     (map #(Integer/parseInt %)))))))
