(ns aoc.day25
  (:require [aoc.shared :refer [read-lines]]))

(def rows 137)
(def cols 139)

(def input
  (->> (read-lines "day25.txt")
       (map-indexed vector)
       (mapcat (fn [[r l]] (map-indexed (fn [c v] [[r c] v]) l)))
       (remove #(#{\.} (second %)))
       (into {})))

(defn can-move? [herd type axis wrap]
  (fn [loc v]
    (let [spot (update loc axis #(mod (inc %) wrap))]
      (cond
        (not= type v) false
        (contains? herd spot) false
        :else spot))))

(defn move [[edit? herd] type axis wrap]
  (let [get-move (can-move? herd type axis wrap)]
    (reduce-kv (fn [[edit? herd] k v]
                 (if-let [spot (get-move k v)]
                   [(or edit? true) (assoc herd spot v)]
                   [edit? (assoc herd k v)]))
               [edit? {}]
               herd)))

(defn print-herd [[_ herd]]
  (doseq [r (range rows)]
    (doseq [c (range cols)]
      (print (herd [r c] \.)))
    (println)))

(defn step [[_edit? herd]]
  (-> [false herd]
      (move \> 1 cols)
      (move \v 0 rows)))

;Part 1
(->> [true input]
     (iterate step)
     (take-while #(first %))
     count)