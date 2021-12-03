(ns advent.day2
  (:require [advent.utils :as utils]
            [clojure.string :as string]))

(def init-pos
  {:depth 0
   :horiz 0
   :aim 0})

(defn parse-move
  "Returns a tuple of the `movement` as 
   [:direction count]"
  [movement]
  (let [[str-dir str-count] (string/split movement #" ")]
    [(keyword str-dir)
     (Integer/parseInt str-count)]))

(defn move-to-change-basic
  "Returns a map of movement as change to either :horiz or :depth"
  [[dir count]]
  (case dir
    :forward {:horiz count}
    :down {:depth count}
    :up {:depth (* -1 count)}))

(defn aimed-move-to-change
  "Returns a map of the change based on aim."
  [[type change] pos]
  (case type
    :down {:aim change}
    :up {:aim (* -1 change)}
    :forward {:horiz change
              :depth (* (:aim pos) change)}))

(comment
  ;; Part 2
  (let [moves (->> (utils/file-to-seq "input/day2.txt")
                   (map parse-move))
        updated-pos (reduce (fn [pos move]
                              (merge-with +
                                          pos
                                          (aimed-move-to-change move pos)))
                            init-pos
                            moves)]
    (apply * (map updated-pos
                  [:depth :horiz])))

  ;; Part 1
  (let [changes (->> (utils/file-to-seq "input/day2.txt")
                     (map parse-move)
                     (map move-to-change-basic))
        updated-pos (reduce (fn [current change]
                              (merge-with +
                                          current
                                          change))
                            init-pos
                            changes)]
    (apply * (vals updated-pos))))