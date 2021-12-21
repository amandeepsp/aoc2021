(ns aoc.day03
  (:require [aoc.shared :refer [read-lines str->int]]
            [clojure.string :as str]))

(def input (->> (read-lines "day03.txt")
                (map (fn [x]
                       (map #(Character/digit % 10) (vec x))))))

(defn transpose [x]
  (apply mapv vector x))

(def transpose-input (transpose input))

(def num-records (count (first transpose-input)))

(def num-ones
  (map (partial apply +) transpose-input))

(def num-zeros (map #(- num-records %) num-ones))

(defn gamma [num-one num-zero]
  (if (>= num-one num-zero)
    \1 \0))

(defn epsilon [num-one num-zero]
  (if (< num-one num-zero)
    \1 \0))

(defn calc-rate [kind]
  (-> (map kind num-ones num-zeros)
      (str/join)
      (str->int 2)))

;Part 1
(* (calc-rate gamma) (calc-rate epsilon))

(defn scan-rate [kind]
  (loop [records input
         number []
         i 0]
    (if (= (count records) 1)
      (into number (drop i (first records)))
      (let [transpose-records (transpose records)
            num-records (count (nth transpose-records i))
            num-ones (apply + (nth transpose-records i))
            num-zeros (- num-records num-ones)
            dominant-char (kind num-ones num-zeros)]
        (recur
         (filter (fn [x]
                   (= (nth x i) (Character/digit dominant-char 10)))
                 records)
         (into number [dominant-char])
         (inc i))))))

(defn calc-rate2 [kind]
  (-> (scan-rate kind)
      (str/join)
      (str->int 2)))

;Part 2
(* (calc-rate2 gamma) (calc-rate2 epsilon))

