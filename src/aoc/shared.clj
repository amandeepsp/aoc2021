(ns aoc.shared
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn str->int
  ([x] (Long/parseLong x))
  ([x radix] (Long/parseLong x radix)))

(defn read-lines [file-resource]
  (->> (io/resource file-resource)
       (slurp)
       (str/split-lines)))

(defn transpose [x]
  (apply mapv vector x))

(defn abs [x]
  (if (< x 0) (- x) x))

(defn take-upto
  "Returns a lazy sequence of successive items from coll until
   (pred item) returns true, including that item. pred must be
   free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) nil)
       (cons (first s) (take-upto pred (rest s)))))))
