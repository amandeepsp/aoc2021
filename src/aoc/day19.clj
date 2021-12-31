(ns aoc.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc.shared :refer [str->int abs]]))

(defn parse-scanner [s]
  (let [[name & bs] (str/split-lines s)
        i (->> (re-seq #"\d+" name) first str->int)]
    {:scanner i
     :beacons (->> (mapcat #(re-seq #"-?\d+" %) bs)
                   (map str->int)
                   (partition 3)
                   set)}))

(defn parse-input [s]
  (->> (str/split s #"\R\R")
       (map parse-scanner)))

(def scanners
  (-> (io/resource "day19.txt")
      slurp
      parse-input))

(defn all-orientations [[x y z]]
  (concat
   (for [[x y z] [[x y z] [y z x] [z x y]]
         [sx sy sz] [[+ + +] [+ - -] [- + -] [- - +]]]
     [(sx x) (sy y) (sz z)])
   (for [[x y z] [[x z y] [z y x] [y x z]]
         [sx sy sz] [[+ - +] [+ + -] [- + +] [- - -]]]
     [(sx x) (sy y) (sz z)])))

(defn offset
  [bs1 bs2]
  (->> (for [b1 bs1 b2 bs2] (map - b1 b2))
       (group-by identity)
       vals
       (some #(when (<= 12 (count %)) (first %)))))

(defn try-offsets
  [s1 s2]
  (some
   #(when-let [offset (offset (:beacons s1) %)]
      {:scanner (:scanner s2)
       :offset offset
       :beacons (map (fn [x] (map + offset x)) %)})
   (->> (map all-orientations (:beacons s2)) (apply map vector))))

(defn solve [[s0 & ss]]
  (loop [base s0
         offsets [{:scanner 0 :offset [0 0 0]}]
         [curr & rest] ss
         fail ()]   ; Place to keep un-found scanners.
    (if curr
      (if-let [found (try-offsets base curr)]
        (recur (update base :beacons into (:beacons found))
               (conj offsets (select-keys found [:scanner :offset]))
               (into rest fail)   ; Reset all un-found scanners.
               ())
        (recur base offsets rest (conj fail curr)))
      {:beacons (:beacons base) :offsets offsets :fail fail})))

;Part 1
(-> scanners solve :beacons count)

;Part 2
(let [locs (->> (solve scanners)
                :offsets
                (map :offset))]
  (->> (for [a locs b locs :when (not= a b)]
         (->> (map - a b) (map abs) (reduce +)))
       (reduce max)))