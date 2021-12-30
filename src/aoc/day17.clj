(ns aoc.day17)

(def x-bounds [79 137])
(def y-bounds [-176 -117])

(defn vec-add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn neg-sgn [x]
  (cond
    (= x 0) 0
    (> x 0) -1
    (< x 0) 1))

(defn ds [v0] v0)

(defn dv [[vx _]]
  [(neg-sgn vx) -1])

(defn next-s [[s0 v0]]
  [(vec-add s0 (ds v0))
   (vec-add v0 (dv v0))])

(defn in-target? [[x y]]
  (and
   (<= (first x-bounds) x (second x-bounds))
   (<= (first y-bounds) y (second y-bounds))))

(defn beyond-target? [[x y]]
  ;Points of no recovery
  (or
   (< y (first y-bounds))
   (> x (second x-bounds))))

(defn is-valid-tragectory? [v0]
  (loop [s [0 0]
         v v0
         path [s]]
    (cond
      (beyond-target? s) nil
      (in-target? s) path
      :else (let [[s' v'] (next-s [s v])]
              (recur s' v' (conj path s'))))))

(def candidate-velocities
  (for [x (range 1 (inc (second x-bounds)))
        y (range (first y-bounds) (inc (- (first y-bounds))))]
    [x y]))

(defn debug-count [x]
  (println "Part 2:" (count x))
  x)

(->> candidate-velocities
     (map is-valid-tragectory?)
     (filter (complement nil?))
     (debug-count); Part 2
     (mapcat identity)
     (map second)
     (apply max)); Part 1
