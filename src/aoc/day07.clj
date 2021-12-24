(ns aoc.day07
  (:require [clojure.java.io :as io]
            [aoc.shared :refer [abs]]))

(def positions
  (read-string (str "[" (slurp (io/resource "day07.txt")) "]")))

(defn median [coll]
  (let [sorted-coll (sort coll)
        len (count coll)
        mid (int (/ len 2))]
    (if (odd? len)
      (nth sorted-coll mid)
      [(nth sorted-coll (dec mid)) (nth sorted-coll mid)])))

(defn abs-cost [final-pos pos]
  (abs (- pos final-pos)))

(defn calc-cost [positions cost-fn final-pos]
  (apply + (map (partial cost-fn final-pos) positions)))

; Part 1
(let [pos-median (median positions)]
  (if (coll? pos-median)
    (apply min (map (partial calc-cost positions abs-cost) pos-median))
    (calc-cost positions abs-cost pos-median)))

(defn mean [coll]
  (/ (apply + coll) (count coll)))

(defn square-cost [final-pos pos]
  (let [dist (abs (- pos final-pos))]
    (/ (* dist (inc dist)) 2)))

; Part 2
(let [final-pos (int (mean positions))]
  (calc-cost positions square-cost final-pos))




