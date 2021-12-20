(ns advent.day11
  (:require [advent.utils :as utils]))

(defrecord Point [x y])

(defn update-2d
  "Applies `f` to all points or to a coll of `points` in `seq-seqs`"
  ([seq-seqs f]
   (map #(map f %) seq-seqs))
  ([seq-seqs f points]
   (reduce (fn [acc point]
             (update-in acc [(:y point) (:x point)] f))
           seq-seqs
           points)))

(defn adjacent
  "Returns a coll of points adjacent (including diagonal) to `point`"
  [point]
  (->> [[-1 -1] ; top left
        [0 -1] ; top
        [1 -1] ; top right
        [-1 0] ; left
        [1 0] ; right
        [-1 1] ; bottom left
        [0 1] ; bottom
        [1 1] ; bottom right
        ]
       (map (fn [[x y]]
              (Point. (+ x (:x point))
                      (+ y (:y point)))))))

(defn to-glow
  "returns a list of Points in `seq-seqs` that are greater than 9."
  [seq-seqs]
  (->> seq-seqs
       (utils/map-indexed-2d
        (fn [[x y] e]
          (if (< e 10)
            nil
            (Point. x y))))
       flatten
       (filter (complement nil?))))

(defn valid-in?
  "Returns true if `point` is within the bounds of `seq-seqs`"
  [seq-seqs point]
  (and (>= (-> seq-seqs count dec)
           (:y point)
           0)
       (>= (-> seq-seqs (nth (:y point) '()) count dec)
           (:x point)
           0)))

(defn glow
  "Sets all elements in octopi above 9 to 0 and increments 
   all adjacent points, setting them to 0 if they also pass 9."
  [octopi]
  (loop [unlit-oct octopi]
    (let [left-to-glow (to-glow unlit-oct)]
      (if (empty? left-to-glow)
        unlit-oct
        (let [lit-oct (->> unlit-oct
                           (utils/map-2d (fn [e]
                                           (if (> e 9)
                                             0
                                             e)))
                           (map vec)
                           vec)
              incd-adj (->> left-to-glow
                            (map adjacent)
                            flatten
                            (filter (partial valid-in? lit-oct))
                            (reduce (fn [oct to-glow]
                                      (update-in oct
                                                 [(:y to-glow) (:x to-glow)]
                                                 (fn [e]
                                                   (if (not= 0 e)
                                                     (inc e)
                                                     e))))
                                    lit-oct))]
          (recur incd-adj))))))

(comment
;; part 2
  (let [input (->> "input/day11.txt"
                   utils/file-to-seq
                   (map utils/number-seq))]
    (loop [octopi input
           all-flashed false
           step 0
           max 2000]
      (if (or all-flashed
              (= 0 max))
        step
        (let [new-oct (-> octopi
                          (update-2d inc)
                          glow)]
          (recur new-oct
                 (every? #(every? zero? %) new-oct)
                 (inc step)
                 (dec max))))))
;; part 1
  (let [input (->> "input/day11.txt"
                   utils/file-to-seq
                   (map utils/number-seq))]
    (loop [octopi input
           flashes 0
           max 100]
      (if (= 0 max)
        flashes
        (let [new-oct (-> octopi
                          (update-2d inc)
                          glow)]
          (recur new-oct
                 (+ flashes (->> new-oct
                                 flatten
                                 (filter zero?)
                                 count))
                 (dec max)))))))




