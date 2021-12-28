(ns aoc.day15
  (:require [aoc.shared :refer [read-lines]]
            [clojure.data.priority-map :refer [priority-map]]))

(def input
  (->> (read-lines "day15.txt")
       (mapv (fn [x]
               (mapv #(Character/digit % 10) (vec x))))))

(def H (count input))
(def W (count (first input)))

(defn valid-neighbors [[i j] h w]
  (let [dirs [[0 1] [1 0] [0 -1] [-1 0]]]
    (->> dirs
         (map (fn [[dy dx]] [(+ i dy) (+ j dx)]))
         (filter (fn [[i' j']] (and (<= 0 i' (dec h)) (<= 0 j' (dec w))))))))

(def all-coords
  (into {} (for [i (range H)
                 j (range W)]
             [[i j] (get-in input [i j])])))

(defn dijkstra [graph start-coords h w]
  (loop [q (priority-map start-coords 0)
         costs {}]
    (if (empty? q)
      costs
      (let [[node curr-dist] (peek q)
            dist (->> (valid-neighbors node h w)
                      (filter (complement costs))
                      (map #(vector % (+ (graph %) curr-dist)))
                      (into {}))]
        (recur
         (merge-with min (pop q) dist)
         (assoc costs node curr-dist))))))

(defn end-cost [graph h w]
  (-> graph
      (dijkstra [0 0] h w)
      (get [(dec h) (dec w)])))

;Part 1
(println
 "Part 1:"
 (end-cost all-coords H W))

(defn wrap [value]
  (if (>= value 10) (inc (mod value 10)) value))

(defn new-coords-map [[di dj]]
  (->> all-coords
       (map (fn [[[i' j'] val]]
              [[(+ i' (* di H)) (+ j' (* dj W))]
               (wrap (+ val di dj))]))
       (into {})))

(def expanded-input
  (let [cells (for [x (range 5) y (range 5)
                    :when (not (and (= x 0) (= y 0)))]
                [x y])]
    (reduce (fn [exp-input cell]
              (merge exp-input (new-coords-map cell)))
            all-coords cells)))

(def new-H (* 5 H))
(def new-W (* 5 W))

(println
 "Part 2:"
 (end-cost expanded-input new-H new-W))