(ns aoc.shared
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn str->int [x]
  (Long/parseLong x))

(defn read-lines [file-resource]
  (->> (io/resource file-resource)
       (slurp)
       (str/split-lines)))