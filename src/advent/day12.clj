(ns advent.day12
  (:require [advent.utils :as utils]
            [clojure.set :as s]))

(defn input-to-graph [input-file]
  (->> input-file
       utils/file-to-seq
       (map #(clojure.string/split % #"-"))
       (map (fn [[from to]]
              [[to from] [from to]])) ;; tunnels go both ways
       (reduce into
               []) ;; flatten once
       (filter (fn [[from to]]
                 (and (not= from "end") ;; "end" can't go anywhere
                      (not= to "start")))) ;; can't go to "start"
       (map (fn [[from to]]
              [from #{to}]))
       (map #(apply (partial assoc {}) %))
       (reduce (partial merge-with clojure.set/union) {})))

(comment
  (input-to-graph "input/day12test.txt"))

(defn complete-path? [path]
  (and (->> path first (= "start"))
       (->> path last (= "end"))))

(comment
  [(complete-path? (list "a" "b"))
   (complete-path? (list "start" "end"))])

(defn nodes-leading-to-node [graph node]
  (->> graph
       (filter (fn [[_ connected-nodes]]
                 (contains? connected-nodes node)))
       (map first)))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (nodes-leading-to-node graph "end")))

(defn node-counts [path]
  (reduce (fn [node-counts node]
            (assoc node-counts
                   node
                   (inc (get node-counts node 0))))
          {}
          path))

(comment
  (node-counts (list "A" "b" "A" "end")))

(defn path-violates-count-rules-v1? [path]
  (->> path
       node-counts
       (filter (fn [[node count]]
                 (< 1 count)))
       (map first)
       (some (partial re-matches #".*[a-z].*"))))

(defn path-violates-count-rules-v2? [path]
  (let [small-node-counts
        (->> path
             node-counts
             (filter (fn [[node _]]
                       (re-matches #".*[a-z].*" node))))]
    (or (< 1 (count (filter (fn [[node count]]
                              (< 1 count))
                            small-node-counts)))
        (some (fn [[_ count]]
                (< 2 count))
              small-node-counts))))

(comment
  (path-violates-count-rules-v2? '("A" "b" "c" "A" "c" "c")))

(defn next-steps-in-path [graph current-path]
  (let [new-node (first current-path)
        leading-nodes (nodes-leading-to-node graph new-node)]
    (map #(conj current-path %)
         leading-nodes)))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (next-steps-in-path graph (list "A" "b" "A" "b" "c" "b" "end"))))

(defn next-valid-steps-in-path [graph validator current-path]
  (->> (next-steps-in-path graph current-path)
       (filter validator)))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (next-valid-steps-in-path graph
                              (complement path-violates-count-rules-v2?)
                              (list "A" "b" "A" "b" "c" "b" "end"))))

(defn all-paths [graph start end validator]
  (loop [incomplete-paths #{(list end)}
         complete-paths #{}]
    (if (empty? incomplete-paths)
      complete-paths
      (let [new-paths
            (->> incomplete-paths
                 (map (partial next-valid-steps-in-path
                               graph
                               validator))
                 (map set)
                 (reduce s/union #{}))
            incomplete-paths (->> new-paths
                                  (filter (complement complete-path?))
                                  set)
            new-complete-paths (s/union complete-paths
                                        (->> new-paths
                                             (filter complete-path?)
                                             set))]
        (if (empty? incomplete-paths)
          new-complete-paths
          (recur incomplete-paths
                 new-complete-paths))))))

(comment
  (let [graph (input-to-graph "input/day12.txt")]
    [;; v1
    (count (all-paths graph "start" "end" (complement path-violates-count-rules-v1?)))
    ;; v2
    (count (all-paths graph "start" "end" (complement path-violates-count-rules-v2?)))]))
