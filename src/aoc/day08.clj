(ns aoc.day08
  (:require [aoc.shared :refer [read-lines str->int]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[patterns nums] (str/split line #" \| ")]
    {:pattern (str/split patterns #" ")
     :values (str/split nums #" ")}))

(def input
  (->> (read-lines "day08.txt")
       (map parse-line)))

(def unique-lens #{2 3 4 7}) ; [1 7 4 8]

(def value-freqs
  (->> (mapcat #(get % :values) input)
       (map count)
       (frequencies)))

;Part 1
(apply + (map #(get value-freqs %) unique-lens))

; aaaa
;b    c
;b    c
; dddd
;e    f
;e    f
; gggg

(def decode-map
  {#{\a \b \c \e \f \g}    \0
   #{\c \f}                \1
   #{\a \c \d \e \g}       \2
   #{\a \c \d \f \g}       \3
   #{\b \d \c \f}          \4
   #{\a \b \d \f \g}       \5
   #{\a \b \d \e \f \g}    \6
   #{\a \c \f}             \7
   #{\a \b \c \d \e \f \g} \8
   #{\a \b \c \d \f \g}    \9})

(defn str-diff [s1 s2]
  (let [set1 (into #{} s1)
        set2 (into #{} s2)]
    (set/difference set1 set2)))

(defn str-intersection [s1 s2]
  (let [set1 (into #{} s1)
        set2 (into #{} s2)]
    (set/intersection set1 set2)))

(defn extract-unique [pattern]
  (->> pattern
       (filter #(contains? unique-lens (count %)))
       (sort-by count)))

(defn pin-vals [pattern pairs]
  ; 6 counted digits (0, 6, 9) have only one intersection
  ; with pairs
  (let [first-val (->> (filter #(= 6 (count %)) pattern)
                   (map (partial str-intersection pairs))
                   (filter #(= 1 (count %)))
                   (first))]
    [(first (str-diff pairs first-val))
     (first first-val)]))

(defn decode-7seg [pattern]
  (let [[one seven four eight] (extract-unique pattern)
        cf-vals one
        a-val (first (str-diff seven one))
        bd-vals (str-diff four one)
        eg-vals (str-diff eight (concat [a-val] cf-vals bd-vals))
        [c-val f-val] (pin-vals pattern cf-vals)
        [d-val b-val] (pin-vals pattern bd-vals)
        [e-val g-val] (pin-vals pattern eg-vals)]
    {c-val \c
     f-val \f
     a-val \a
     b-val \b
     d-val \d
     e-val \e
     g-val \g}))

(defn encode-num [decoding str]
  (let [sgns-set (into #{} str)]
    (into #{} (map #(get decoding %) sgns-set))))

(defn decode-line [line]
  (let [encode-map (decode-7seg (:pattern line))]
    (->> (:values line)
         (map (partial encode-num encode-map))
         (map #(get decode-map %))
         (str/join)
         (str->int))))

;Part 2
(reduce + (map decode-line input))