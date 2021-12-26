(ns aoc.day10
  (:require [aoc.shared :refer [read-lines]]
            [clojure.string :as str]))

(def input
  (->> (read-lines "day10.txt")
       (map vec)))

(def opening-brackets [\( \[ \{ \<])
(def closing-brackets [\) \] \} \>])

(def opening-bracket (zipmap closing-brackets opening-brackets))
(def closing-bracket (zipmap opening-brackets closing-brackets))

(def score-map
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(defn update-bracket-stack [stack bracket]
  (if (contains? (set opening-brackets) bracket)
    (conj stack bracket)
    (let [curr-bracket (peek stack)
          expected-bracket (opening-bracket bracket)]
      (if (= curr-bracket expected-bracket)
        (pop stack)
        (reduced {:first-error bracket})))))

; Part 1
(->> (map #(reduce update-bracket-stack '() %) input)
     (filter map?)
     (map :first-error)
     (map score-map)
     (apply +))

(defn autocomplete [stack]
  (loop [curr-stack stack
         acc []]
    (let [curr-bracket (peek curr-stack)]
      (if (empty? curr-stack)
        acc
        (recur (pop curr-stack)
               (conj acc (closing-bracket curr-bracket)))))))

(def score2-map
  {\) 1
   \] 2
   \} 3
   \> 4})

(defn socre2 [completion]
  (loop [coll completion
         score 0]
    (if (empty? coll)
      score
      (let [curr-bracket (first coll)]
        (recur
         (rest coll)
         (+ (* 5 score) (score2-map curr-bracket)))))))


(def part2-scores
  (->> (map #(reduce update-bracket-stack '() %) input)
       (filter (complement map?))
       (map autocomplete)
       (map socre2)
       (sort)))

; Part 2
(nth part2-scores (/ (count part2-scores) 2))


