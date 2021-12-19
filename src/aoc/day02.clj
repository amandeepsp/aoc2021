(ns aoc.day02
  (:require [aoc.shared :as shared :refer [read-lines str->int]]
            [clojure.string :as str]))

(def input (->> (read-lines "day02.txt")
                (map (fn [x] (str/split x #" ")))
                (map (fn [[cmd num]] 
                       [(keyword cmd) (str->int num)]))))

(defn next-pos [[x y] [cmd amt]]
  (case cmd
    :forward [(+ x amt) y]
    :up [x (- y amt)]
    :down [x (+ y amt)]))

(def final-pos (reduce next-pos [0 0] input))

;Part 1
(apply * final-pos)

(defn next-pos2 [[x y aim] [cmd amt]]
  (case cmd
    :forward [(+ x amt) (+ y (* aim amt)) aim]
    :up [x y (- aim amt)]
    :down [x y (+ aim amt)]))

(def final-pos2 (reduce next-pos2 [0 0 0] input))

;Part 2
(* (first final-pos2) (second final-pos2))


