(ns aoc.day20
  (:require [aoc.shared :refer [read-lines transpose]]))

(defn parse-input [line-seq]
  (let [[enhance _ & img] (map #(mapv {\. 0 \# 1} %) line-seq)]
    {:enhance enhance
     :img     (vec img)}))

(def data
  (-> (read-lines "day20.txt") parse-input))

(defn convolve [row factor]
  (map #(+ %3 (* factor (+ %2 (* factor %1)))) row (drop 1 row) (drop 2 row)))

(defn pad [m pad-val pad-s]
  (let [s (+ (count m) (* 2 pad-s)) ; Assume square imgs/matrices
        padding-rows (repeat pad-s (repeat s pad-val))
        padding-col (vec (repeat pad-s pad-val))]
    (concat padding-rows
            (map (fn [r] (concat padding-col r padding-col)) m)
            padding-rows)))

(defn enhance-once [enhance img pad-char]
  (as-> img $
    (pad $ pad-char 2)
    (map #(convolve % 2) $)
    (transpose $)
    (map #(convolve % 8) $)
    (transpose $)
    (map #(map enhance %) $)))

(defn enhance-twice [enhance img]
  (as-> img $
    (enhance-once enhance $ 0)
    (enhance-once enhance $ (enhance 0))))

(defn solve [{:keys [img enhance]} times]
  (->> img
       (iterate (partial enhance-twice enhance))
       (drop (/ times 2))
       first
       (flatten)
       (reduce +)))

;Part 1
(solve data 2)

;Part 2
(solve data 50)