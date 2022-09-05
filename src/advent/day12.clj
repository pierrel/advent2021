(ns advent.day12
  (:require [advent.utils :as utils]
            [clojure.set :as s]))

(defn input-to-graph [input-file]
  (->> input-file
       utils/file-to-seq
       (map #(clojure.string/split % #"-"))
       (map (fn [[from to]]
              [from #{to}]))
       (map #(apply (partial assoc {}) %))
       (reduce (partial merge-with clojure.set/union) {})))

(defn nodes-leading-to-node [graph node]
  (->> graph
       (filter (fn [[_ connected-nodes]]
                 (contains? connected-nodes node)))
       (map first)))

(defn next-steps-in-path [graph current-path]
  (let [new-node (first current-path)
        leading-nodes (nodes-leading-to-node graph new-node)]
    (map #(conj current-path %)
         leading-nodes)))

(defn all-paths [graph start end]
  (loop [paths #{(list end)}]
    (let [incomplete-paths (filter #(not= start
                                          (first %))
                                   paths)
          complete-paths (s/difference paths incomplete-paths)]
      (if (empty? incomplete-paths)
        paths
        (let [new-paths (map (partial next-steps-in-path graph)
                             incomplete-paths)]
          (recur (->> new-paths
                      (map set)
                      (reduce s/union complete-paths))))))))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (all-paths graph "start" "end")))


(comment
  (let [graph (input-to-graph "input/day12test.txt")
        leading-to-end (nodes-leading-to-node graph "end")
        paths (set (map list leading-to-end))
        new-paths (map (partial next-steps-in-path graph) paths)
        ]
    (->> new-paths
         (map set)
         (reduce s/union #{}))
    ))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (nodes-connected-to-node graph "end")))
