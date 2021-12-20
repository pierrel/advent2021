(ns advent.day12
  (:require [advent.utils :as utils]))

(defn input-to-graph [input-file]
  (->> input-file
       utils/file-to-seq
       (map #(clojure.string/split % #"-"))
       (map (fn [[from to]]
              [from #{to}]))
       (map #(apply (partial assoc {}) %))
       (reduce (partial merge-with clojure.set/union) {})))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    graph))