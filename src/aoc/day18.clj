(ns aoc.day18
  (:require [clojure.zip :as z]
            [clojure.walk :as walk]
            [aoc.shared :refer [read-lines]]))

(def input
  (->> (read-lines "day18.txt")
       (map read-string)))

(defn leaves-seq [number get-next-fn]
  (->> number
       (iterate get-next-fn)
       (drop 1)
       (take-while (every-pred coll? (complement z/end?)))
       (filter (complement z/branch?))))

(defn root-loc [number]
  (->> number
       (iterate z/up)
       (take-while identity)
       last))

(defn next-leaves [number] (leaves-seq number z/next))
(defn prev-leaves [number] (leaves-seq number z/prev))

(defn explode [number]
  (if-let [explode-loc
           (->> number
                next-leaves
                (filter #(> (count (z/path %)) 4))
                first
                z/up)]
    (let [[left-val right-val] (z/node explode-loc)
          explode-loc (z/replace explode-loc 0)
          explode-loc (if-let [left-loc (first (prev-leaves explode-loc))]
                        (-> left-loc (z/edit + left-val) next-leaves first)
                        explode-loc)
          explode-loc (if-let [right-loc (first (next-leaves explode-loc))]
                        (-> right-loc (z/edit + right-val) prev-leaves first)
                        explode-loc)]
      [explode-loc :continue])
    [number :done]))

(defn full-explode [number]
  (->> [number :continue]
       (iterate (fn [[number _]] (explode number)))
       (map-indexed vector)
       (filter (fn [[_ [_ state]]] (= state :done)))
       first ;first done
       ((fn [[i [number _]]]
          [number (if (> i 1) :changed :done)]))))

(defn split [number]
  (if-let [split-loc (->> number
                          next-leaves
                          (filter (fn [loc] (>= (z/node loc) 10)))
                          first)]
    [(-> split-loc
         (z/edit (fn [n] [(quot n 2) (+ (quot n 2) (rem n 2))])))
     :changed]
    [number :done]))

(defn normalize [number]
  (let [[exploded e-state] (full-explode number)
        [split-n s-state] (split (root-loc exploded))]
    (if (= :done e-state s-state)
      (z/root split-n)
      (recur (root-loc split-n)))))

(defn add [n1 n2]
  (normalize (z/vector-zip [n1 n2])))

(defn magnitude [number]
  (walk/postwalk
   (fn [node] (if (number? node)
                node
                (+ (* 3 (first node)) (* 2 (second node)))))
   number))

;Part 1
(magnitude (reduce add input))

;Part 2
(->> (for [n1 input n2 input :when (not= n1 n2)]
       (magnitude (add n1 n2)))
     (reduce max))