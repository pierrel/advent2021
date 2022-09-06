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

(defn nodes-leading-to-node [graph node]
  (->> graph
       (filter (fn [[_ connected-nodes]]
                 (contains? connected-nodes node)))
       (map first)))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (nodes-leading-to-node graph "end")))

(defn next-steps-in-path [graph current-path]
  (let [new-node (first current-path)
        leading-nodes (nodes-leading-to-node graph new-node)]
    (map #(conj current-path %)
         leading-nodes)))

(comment
  (let [graph (input-to-graph "input/day12test.txt")]
    (next-steps-in-path graph (list "A" "end"))))

(defn node-counts [path]
  (reduce (fn [node-counts node]
            (assoc node-counts
                   node
                   (inc (get node-counts node 0))))
          {}
          path))

(comment
  (node-counts (list "A" "b" "A" "end")))


(defn path-has-multiple? [path no-multiples]
  (let [mult-set (set no-multiples)
        counts (node-counts path)]
    (some (fn [no-multiple]
            (< 1 (get counts no-multiple 0)))
          no-multiples)))

(defn path-has-multiple-forbidden? [path]
  (->> path
       node-counts
       (filter (fn [[node count]]
                 (< 1 count)))
       (map first)
       (some (partial re-matches #".*[a-z].*"))))

(comment
  (path-has-multiple-forbidden? '("A" "b" "c" "A")))

(comment
  (path-has-multiple? (list "A" "b" "b" "end")
                      (list "A" "b")))

(defn paths-without-multiple [paths no-multiples]
  (filter #(not (path-has-multiple? % no-multiples))
          paths))

(comment
  (paths-without-multiple (list '("A" "b" "end")
                                '("A" "c" "A" "b" "end")
                                '("A" "b" "A" "b" "end"))
                          (list "b")))

(defn all-paths [graph start end]
  (loop [paths #{(list end)}]
    (let [incomplete-paths (filter #(not= start
                                          (first %))
                                   paths)
          complete-paths (s/difference paths incomplete-paths)]
      (if (empty? incomplete-paths)
        complete-paths
        (let [new-paths
              (as-> incomplete-paths ps
                (map (partial next-steps-in-path graph) ps)
                (map set ps)
                (reduce s/union #{} ps)
                (filter (complement path-has-multiple-forbidden?)
                        ps)
                (set ps))]
          (if (empty? (s/difference new-paths
                                    (set incomplete-paths)))
            complete-paths
            (recur (s/union new-paths complete-paths))))))))

(comment
  (let [graph (input-to-graph "input/day12test2.txt")]
    (count (all-paths graph "start" "end"))))


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
