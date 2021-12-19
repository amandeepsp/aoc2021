(ns aoc.day01
  (:require [aoc.shared :as shared :refer [read-lines, str->int]]))

(def input (map str->int (read-lines "day01.txt")))

(defn num-increases [inputs]
  (->> (map - (rest inputs) inputs)
       (filter pos-int?)
       (count)))

;Part 1
(num-increases input)

(def sliding-window-input (->> (partition 3 1 input)
                               (map (partial apply +))))

;Part 2
(num-increases sliding-window-input)


