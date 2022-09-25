(ns advent.day14
  (:require [advent.utils :as utils]))

(defn input-to-instructions [file]
  (->> file
       utils/file-to-seq
       (filter #(re-matches #".*->.*" %))
       (map #(clojure.string/split % #" -> "))))

(defn input-to-polymer [file]
  (->> file
       utils/file-to-seq
       first))

(comment
  (input-to-instructions "input/day14test.txt")
  (input-to-polymer "input/day14test.txt"))

(defn pairs [polymer]
  (let [bases (clojure.string/split polymer
                                    #"")]
    (->> (interleave bases
                     (rest bases))
         (partition 2)
         (map clojure.string/join))))

(comment
  (pairs "NNGG"))
