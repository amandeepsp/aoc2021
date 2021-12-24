(ns aoc.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [aoc.shared :refer [str->int]]))

(def input
  (->> (io/resource "day06.txt")
       (slurp)
       ((fn [num] (str/split num #",")))
       (map str->int)))

(defn floor-dec [x]
  (if (= x 0) 6 (dec x)))

(defn next-state [curr-state]
  ;(println (count curr-state) ":" curr-state)
  (let [num-zeros (count (filter zero? curr-state))]
    (concat
      (map floor-dec curr-state)
      (repeat num-zeros 8))))

;Part 1
(->> (iterate next-state input)
     (take 81)
     (last)
     (count))


(def fish-count
  (memoize 
   (fn [days curr-age]
     (if (<= days curr-age)
       1
       (+ (fish-count (- days curr-age 1) 6)
          (fish-count (- days curr-age 1) 8))))))

(defn total-fish-count [days input]
  (apply + (map (partial fish-count days) input)))

;Part 2
(total-fish-count 256 input)
