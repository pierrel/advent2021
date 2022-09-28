(ns advent.day14
  (:require [advent.utils :as utils]))

(defn input-to-instructions [file]
  (->> file
       utils/file-to-seq
       (filter #(re-matches #".*->.*" %))
       (map #(clojure.string/split % #" -> "))
       (into {})))

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

(defn unpair [pairs]
  (let [splits (map #(clojure.string/split % #"")
                    pairs)]
    (clojure.string/join
     (reduce (fn [acc cur]
               (into acc (rest cur)))
             (first splits)
             (rest splits)))))

(comment
  (-> "NNGG" pairs unpair))

(defn pair-insert [rules pair]
  (if-let [between (get rules pair)]
    (let [split-pair (clojure.string/split pair #"")]
      (clojure.string/join
       [(first split-pair)
        between
        (last split-pair)]))
    pair))

(comment
  (pair-insert {"BN" "H"} "BE")
  (pair-insert {"BN" "H"} "BN"))

(defn mutate [rules template]
  (->> template
       pairs
       (map (partial pair-insert rules))
       unpair))

(defn bubble-up [f lookup-counts]
  (reduce (fn [[max-char max-count] [char count]]
            (if (f max-count count)
              [char count]
              [max-char max-count]))
          lookup-counts))

(defn lookup-char-count [str]
  (reduce (fn [acc cur]
            (let [c (get acc cur 0)]
              (assoc acc cur (inc c))))
          {}
          (seq str)))

(defn most-common [polymer]
  (bubble-up <
             (lookup-char-count polymer)))

(defn least-common [polymer]
  (bubble-up >
             (lookup-char-count polymer)))

(comment
  (most-common "NCCCCCNNGCCGGG")
  (least-common "NCCCCCNNGCCGGG")
  (seq {:a 1 :b 2})
  (seq "abc")
  (merge-with + {} {"a" 2})
  (update {:a 5} :a + 1))

(defn follow [rules pair]
  (if-let [following (get rules pair)]
    (str (first pair)
         following)))

(comment
  (follow {"NN" "B"} "NN")
  (let [rules (input-to-instructions "input/day14test.txt")
        starters (keys rules)]
    (map #(take 10 (iterate (partial follow rules)
                            %))
         starters)))

(defn until-cycle [col]
  (loop [cur (first col)
         rem (rest col)
         uncycled []
         lookup {}]
    (let [maybe-cycled (conj uncycled cur)]
      (if (get lookup cur)
        maybe-cycled
        (recur (first rem)
               (rest rem)
               maybe-cycled
               (assoc lookup cur true))))))

(comment
  (let [rules (input-to-instructions "input/day14test.txt")
        starters (keys rules)]
    (->> starters
         (map #(iterate (partial follow rules) %))
         (map until-cycle)
         (interleave starters)
         (partition 2)
         (map vec)
         (into {}))))

(defn counts-after-iterations [rules iterations template]
  (let [template-with-iterations (->> template
                                      seq
                                      (map #(vector % iterations)))]
    (loop [rem template-with-iterations
           counts (->> template-with-iterations
                       (map first)
                       (map #(hash-map % 1))
                       (reduce (partial merge-with +)))]
      (if (> 2 (count rem))
        counts
        (let [[top iterations] (first rem)
              adjacent (-> rem second first)
              pair (str top adjacent)]
          (if (= iterations 0)
            (recur (rest rem)
                   counts)
            (if-let [result (first (get rules pair))]
              (let [new-counts (assoc counts
                                      result
                                      (inc (get counts result 0)))
                    new-iterations (dec iterations)
                    new-poly [result new-iterations]]
                (recur (conj (conj (rest rem) new-poly)
                             [top new-iterations])
                       new-counts))
              (recur (rest rem)
                     counts))))))))


;; PART 2
(comment
  (let [file "input/day14test.txt"
        template (->> file
                      input-to-polymer)
        rules (input-to-instructions file)
        nn (first (pairs template))
        to-ten (map #(vector % (counts-after-iterations rules % nn))
                    (range 0 11))]
    (->> (interleave to-ten
                     (rest to-ten))
         (partition 2)
         (map (fn [[[l-idx, left] [r-idx, right]]]
                (vector (str l-idx " -> " r-idx)
                        (merge-with - left right)))))))

(comment
  (let [file "input/day14test.txt"
        iterations 40
        template (->> file
                      input-to-polymer)
        rules (input-to-instructions file)
        counts (counts-after-iterations rules iterations template)
        max-mol (bubble-up < counts)
        min-mol (bubble-up > counts)]
    (- (second max-mol)
       (second min-mol))))


;; PART 1
(comment
  (let [file "input/day14test.txt"
        template (input-to-polymer file)
        rules (input-to-instructions file)
        polymer (nth (iterate (partial mutate rules)
                              template)
                     10)]
    (- (-> polymer most-common last)
       (-> polymer least-common last))))
