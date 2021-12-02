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

(defn move-to-change
  "Returns a map of movement as change to either :horiz or :depth"
  [[dir count]]
  (case dir
    :forward {:horiz count}
    :down {:depth count}
    :up {:depth (* -1 count)}))

(comment
  ;; Part 1
  (let [changes (->> (utils/file-to-seq "input/day2.txt")
                     (map parse-move)
                     (map move-to-change))
        updated-pos (reduce (fn [current change]
                              (merge-with +
                                          current
                                          change))
                            init-pos
                            changes)]
    (apply * (vals updated-pos))))