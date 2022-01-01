(ns aoc.day22
  (:require [aoc.shared :refer [read-lines str->int]]
            [clojure.string :as str]))

(defn parse-line [line]
  [(str/starts-with? line "on")
   (mapv str->int (re-seq #"-?\d+" line))])

(def input
  (->> "day22.txt" read-lines (mapv parse-line)))

(defn negate-intersect
  "Add a negation zone for the intersection between the cuboids"
  [[_ [ax1 ax2 ay1 ay2 az1 az2]]
   zones
   [on? [bx1 bx2 by1 by2 bz1 bz2]]]
  (let [x1 (max ax1 bx1), x2 (min ax2 bx2)
        y1 (max ay1 by1), y2 (min ay2 by2)
        z1 (max az1 bz1), z2 (min az2 bz2)]
    (if (and (<= x1 x2) (<= y1 y2) (<= z1 z2))
      (conj zones [(not on?) [x1 x2 y1 y2 z1 z2]])
      zones)))


(defn build-zones
  [zones [on? :as cuboid]]
  (cond-> (reduce #(negate-intersect cuboid %1 %2) zones zones)
    on? (conj cuboid)))

(defn volume
  [[on? [x1 x2 y1 y2 z1 z2]]]
  (* (if on? 1 -1)
     (- (inc x2) x1) (- (inc y2) y1) (- (inc z2) z1)))

(defn solve [adjust]
  (->> input
       adjust
       (reduce build-zones '())
       (map volume)
       (apply +)))


(solve
 (fn [regions]
   (->> regions
        (map (fn [[on? [x1 x2 y1 y2 z1 z2]]]
               (let [x1 (max -50 x1), x2 (min 50 x2)
                     y1 (max -50 y1), y2 (min 50 y2)
                     z1 (max -50 z1), z2 (min 50 z2)]
                 (when (and (<= x1 x2) (<= y1 y2) (<= z1 z2))
                   [on? [x1 x2 y1 y2 z1 z2]]))))
        (filter some?))))


(solve identity)