(ns aoc.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (-> (io/resource "day14.txt")
      (slurp)
      (str/split #"\R\R")))

(def template (first input))

(defn do-subs [[[fst snd] sub]]
  [(str fst snd)
   (str fst sub snd)])

(def sub-rules
  (->> (second input)
       (str/split-lines)
       (map #(str/split % #" -> "))
       (into {})))

(def sub-result-rules
  (->> (map do-subs sub-rules)
       (into {})))

(defn apply-subs [template]
  (let [subs-pairs (->> (partition 2 1 template)
                        (map str/join)
                        (map sub-result-rules))]
    (apply str
           (first subs-pairs)
           (map #(str/join (rest %)) (rest subs-pairs)))))

(defn min-max-diff [coll]
  (- (apply max coll) (apply min coll)))

;Part-1 (naive)
(->> template
     (iterate apply-subs)
     (take 11)
     (last)
     (frequencies)
     (vals)
     (min-max-diff))

(defn create-new-pairs [[k v]]
  [k [(str (first k) v)
      (str v (second k))]])

(def sub-pairs
  (into {} (map create-new-pairs sub-rules)))

(def template-freqs
  (let [keys (map str/join (partition 2 1 template))]
    (reduce (fn [m k]
              (update-in m [k] (fnil inc 0)))
            {} keys)))

(defn update-seq [template-freqs [pair [new-pair1 new-pair2]]]
  (if-let [existing-count (template-freqs pair)]
    [[pair (- existing-count)]
     [new-pair1 existing-count]
     [new-pair2 existing-count]]
    []))

(defn apply-subs2 [template-freqs]
  (let [updates (apply concat 
                       (map (partial update-seq template-freqs) 
                            sub-pairs))]
    (->> updates
         (reduce (fn [m [pair diff]]
                   (update m pair (fnil #(+ % diff) 0)))
                 template-freqs)
         (into {}))))

(defn count-chars [template-freqs]
  (reduce (fn [char-freqs [pair count]]
            (update char-freqs (first pair) (fnil #(+ % count) 0)))
          {(last template) 1}
          template-freqs))

;Part 2
(->> template-freqs
     (iterate apply-subs2)
     (take 41)
     (last)
     (count-chars)
     (vals)
     (min-max-diff))