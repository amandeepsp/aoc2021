(ns aoc.day09
  (:require [aoc.shared :refer [read-lines]]))

(def input
  (->> (read-lines "day09.txt")
       (map (fn [x]
              (map #(Character/digit % 10) (vec x))))
       (to-array-2d)))

(def H (alength input))
(def W (alength (aget input 0)))

(defn neighbors [x y]
  [[(inc x) y]
   [(dec x) y]
   [x (inc y)]
   [x (dec y)]])

(defn valid-point? [[i j]]
  (and (<= 0 i (dec H))
       (<= 0 j (dec W))))

(defn valid-neighbors [i j]
  (filter valid-point? (neighbors i j)))

(defn is-local-minima? [i j]
  (let [valid-neighbors' (valid-neighbors i j)
        min-neighbors
        (apply min (map (fn [[i' j']]
                          (aget input i' j'))
                        valid-neighbors'))
        curr-val (aget input i j)]
    (> min-neighbors curr-val)))

(def local-minimas
  (for [i (range H)
        j (range W)
        :when (is-local-minima? i j)]
    [i j]))

;Part 1
(apply + (map (fn [[i j]]
                (inc (aget input i j)))
              local-minimas))

(defn dfs-neighbors [[i j] visited]
  (->> (valid-neighbors i j)
       (filter (fn [[i' j']]
                 (not= 9 (aget input i' j'))))
       (filter (fn [[i' j']]
                 (not (contains? visited [i' j']))))))


(defn count-dfs [start]
  (loop [visited #{start} frontier [start]]
    (if (empty? frontier)
      (count visited)
      (let [v (peek frontier)
            neighbors (dfs-neighbors v visited)]
        (recur
         (into visited neighbors)
         (into (pop frontier) (remove visited neighbors)))))))

; Part 2
(->> (map count-dfs local-minimas)
     (sort)
     (take-last 3)
     (apply *))

