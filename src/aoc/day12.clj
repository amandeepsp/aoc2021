(ns aoc.day12
  (:require [aoc.shared :refer [read-lines]]
            [clojure.string :as str]))

(def input
  (->> (read-lines "day12.txt")
       (map #(str/split % #"-"))))

(defn edges->adjmap [edges]
  (->> edges
       (reduce (fn [adjmap [e-start e-end]]
                 (-> adjmap
                     (update-in [e-start] conj e-end)
                     (update-in [e-end] conj e-start)))
               {})
       (map (fn [[key val]]
              [key (set val)]))
       (into {})))

(def graph (edges->adjmap input))
(def visited (atom #{}))
(def path-counter (atom 0))

(defn is-small-cave? [id]
  (= id (str/lower-case id)))

(defn count-paths [start]
  (cond (is-small-cave? start)
        (swap! visited conj start))

  (if (= start "end")
    (swap! path-counter inc)
    (doseq [v (graph start)
            :when (not (contains? @visited v))]
      (count-paths v)))

  (cond (is-small-cave? start)
        (swap! visited disj start)))

(count-paths "start")
(println "Part 1:" @path-counter)

(def excluded-nodes #{"start" "end"})

(def visited-map (atom (->> graph
                            (map (fn [[k _]] [k 0]))
                            (into {}))))
(reset! path-counter 0)

(defn valid-neighbor [node]
  (let [visited-count (get @visited-map node 0)
        twice-visited-exists? (some #(= 2 %) (vals @visited-map))]
    (cond
      (excluded-nodes node) (< visited-count 1)
      (is-small-cave? node) (< visited-count (if twice-visited-exists? 1 2))
      :else true)))

(defn count-paths2 [start]
  (cond
    (is-small-cave? start)
    (swap! visited-map update-in [start] inc))

  (if (= start "end")
    (swap! path-counter inc)
    (doseq [v (graph start)
            :when (valid-neighbor v)]
      (count-paths2 v)))

  (cond
    (is-small-cave? start)
    (swap! visited-map update-in [start] dec)))

(count-paths2 "start")
(println "Part 2:" @path-counter)




