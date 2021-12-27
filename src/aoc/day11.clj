(ns aoc.day11
  (:require [aoc.shared :refer [read-lines all-true?]]))

(def input
  (->> (read-lines "day11.txt")
       (mapv (fn [x]
               (mapv #(Character/digit % 10) (vec x))))))

(def H (count input))
(def W (count (first input)))

(defn inc-all [grid]
  (mapv (fn [row]
          (mapv inc row))
        grid))

(def adj-seq
  [[0  1]
   [0 -1]
   [1  0]
   [-1  0]
   [-1 -1]
   [1  1]
   [-1  1]
   [1 -1]])

(defn adjs [[i j]]
  (map (fn [[i' j']]
         [(+ i i') (+ j j')])
       adj-seq))

(defn valid-adjs [coords]
  (->> (adjs coords)
       (filter (fn [[i j]]
                 (and (<= 0 i (dec H))
                      (<= 0 j (dec W)))))))

(def total-flashes (atom 0))
(def flashed (atom #{}))

(defn flash-adj [grid coords]
  (swap! flashed conj coords)
  (swap! total-flashes inc)
  (let [adj-coords (valid-adjs coords)]
    (reduce (fn [grid coords]
              (update-in grid coords inc))
            grid
            adj-coords)))

(defn fixpt [f x] (let [fx (f x)] (if (= x fx) x (recur f fx))))

(defn converge [grid]
  (let [positions (for [i (range H)
                        j (range W)
                        :when (> (get-in grid [i j]) 9)
                        :when (not (contains? @flashed [i j]))]
                    [i j])]
    (reduce (fn [grid' coords] 
              (flash-adj grid' coords))
            grid positions)))

(defn reset-flashed [grid]
  (let [grid' (reduce (fn [lines [i j]]
                        (assoc-in lines [i j] 0))
                      grid
                      @flashed)]
    (reset! flashed #{})
    grid'))

(defn step [grid]
  (reset! flashed #{})
  (->> grid
       (inc-all)
       (fixpt converge)
       (reset-flashed)))

;Part 1
(nth (iterate step input) 100)
@total-flashes

;Part 2
(->> (iterate step input)
     (take-while #(not (all-true? (map zero? (flatten %)))))
     (count))








