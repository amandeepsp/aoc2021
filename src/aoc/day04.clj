(ns aoc.day04
  (:require [aoc.shared :refer [str->int transpose]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (-> (io/resource "day04.txt")
               (slurp)
               (str/split #"\R\R")))

(def called-numbers
  (map str->int (str/split (first input) #",")))

(defn create-board-line [line-str]
  (->> (str/split line-str #" ")
       (filter not-empty)
       (map str->int)))

(def boards
  (->> (rest input)
       (map str/split-lines)
       (map (partial map create-board-line))))

(defn has-won? [board called-numbers]
  (some #(set/subset? (set %) (set called-numbers))
        (into board (transpose board))))

(defn time-to-win [board cumulative-called-nos]
  (count (take-while #(not (has-won? board %)) cumulative-called-nos)))

(defn sort-by-time-to-win [boards cumulative-called-nos]
  (->> boards
       (map (fn [board]
              [(time-to-win board cumulative-called-nos) board]))
       (sort-by first)
       (map second)))

(defn score-winning-state [board cumulative-called-nos]
  (let [win-draws (first (drop-while #(not (has-won? board %)) cumulative-called-nos))]
    (->> (set win-draws)
         (set/difference (set (flatten board)))
         (apply +)
         (* (last win-draws)))))

(def cumulative-called-nos (reductions conj [] called-numbers))
(def sorted-boards (sort-by-time-to-win boards cumulative-called-nos))

;Part 1
(score-winning-state (first sorted-boards) cumulative-called-nos)

;Part 2
(score-winning-state (last sorted-boards) cumulative-called-nos)