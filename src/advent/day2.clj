(ns advent.day2
  (:require [advent.utils :as utils]
            [clojure.string :as string]))

(defn parse-move
  "Returns a tuple of the `movement` as 
   [:direction count]"
  [movement]
  (let [[str-dir str-count] (string/split movement #" ")]
    [(keyword str-dir)
     (Integer/parseInt str-count)]))

(defn move-to-change
  "Returns a tuple of movement as change to either :horiz or :depth"
  [[dir count]]
  (case dir
    :forward [:horiz count]
    :down [:depth count]
    :up [:depth (* -1 count)]))

(comment
  ;; Part 1
  (let [movements (advent.utils/file-to-seq "input/day2.txt")
        changes (map move-to-change
                     (map parse-move movements))
        updated-pos (reduce (fn [current [dir change]]
                              (update current
                                      dir
                                      +
                                      change))
                            {:depth 0 :horiz 0}
                            changes)]
    (apply * (vals updated-pos))))