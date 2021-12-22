(ns aoc.day05
  (:require [aoc.shared :refer [read-lines str->int]]))

(defn parse-line [line]
  (let [[x1 y1 x2 y2] (->> (re-find #"(\d+),(\d+) -> (\d+),(\d+)" line)
                           (rest)
                           (map str->int))]
    [[x1 y1] [x2 y2]]))

(def line-segments (map parse-line (read-lines "day05.txt")))

(defn is-horizontal? [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn is-vertical? [[[x1 _] [x2 _]]]
  (= x1 x2))

(def hv-segments
  (filter #(or (is-horizontal? %) (is-vertical? %)) line-segments))

(defn dir [a b]
  (cond (< a b) inc
        (> a b) dec
        :else identity))

(defn points-in-line [[[x1 y1] [x2 y2]]]
  (let [nxt (fn [[x y]] [((dir x1 x2) x) ((dir y1 y2) y)])]
    (conj (->> (iterate nxt [x1 y1])
               (take-while #(not= % [x2 y2])))
          [x2 y2])))

;Part 1
(->> (mapcat points-in-line hv-segments)
     (frequencies)
     (filter #(> (second %) 1))
     (count))

;Part 2
(->> (mapcat points-in-line line-segments)
     (frequencies)
     (filter #(> (second %) 1))
     (count))