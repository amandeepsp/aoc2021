(ns aoc.day13
  (:require [aoc.shared :refer [str->int]]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day13.txt")
      (slurp)
      (str/split #"\R\R")))

(def points
  (->> (first input)
       (str/split-lines)
       (map #(str/split % #","))
       (map (partial map str->int))))

(defn parse-fold [fold-str]
  (let [[_ axis val] (re-find #"fold along (x|y)=(\d+)" fold-str)]
    [(keyword axis) (str->int val)]))

(def folds
  (->> (second input)
       (str/split-lines)
       (map parse-fold)))

folds

(defn fold-fn [[axis value]]
  (case axis
    :x (fn [[x y]]
         (if (> x value)
           [(- (* 2 value) x) y]
           [x y]))
    :y  (fn [[x y]]
          (if (> y value)
            [x (- (* 2 value) y)]
            [x y]))))

(defn fold [points fold-instr]
  (let [folding-fn (fold-fn fold-instr)]
    (->> points
         (map folding-fn)
         (distinct))))

;Part 1
(->> (fold points (first folds))
     (count))

(def fold-result (set (reduce fold points folds)))

(def W (apply max (map first fold-result)))
(def H (apply max (map second fold-result)))

; Part 2
(doseq [i (range (inc H))]
  (println)
  (doseq [j (range (inc W))]
    (if (fold-result [j i])
      (print "#")
      (print "."))))

