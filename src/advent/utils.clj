(ns advent.utils)

(defn file-to-seq
  "Returns a lazy seq of lines in `filename`"
  [filename]
  (-> filename clojure.java.io/reader line-seq))

(defn comma-sep [s]
  (clojure.string/split s #","))

(defn number-seq [s]
  (as-> s v
    (clojure.string/split v #"")
    (map #(Integer/parseInt %) v)))

(defn map-2d [f seq-seqs]
  (map (fn [seq] 
         (map (fn [e]
                (f e)) 
              seq))
       seq-seqs))

(defn map-indexed-2d [f-xy-e seq-seqs]
 (map-indexed (fn [y-idx seq]
                 (map-indexed (fn [x-idx e]
                                (f-xy-e [x-idx y-idx] e))
                              seq))
               seq-seqs))