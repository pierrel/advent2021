(ns advent.utils)

(defn file-to-seq
  "Returns a lazy seq of lines in `filename`"
  [filename]
  (-> filename clojure.java.io/reader line-seq))