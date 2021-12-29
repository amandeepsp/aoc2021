(ns aoc.day16
  (:require [clojure.string :as str]
            [aoc.shared :refer [take-upto]]
            [clojure.java.io :as io]))

(def VERSION-LENGTH 3)
(def TYPE-ID-LENGTH 3)
(def HEADER-LENGTH (+ VERSION-LENGTH TYPE-ID-LENGTH))
(def BIT-GROUP-SIZE 5)
(def NUM-LEN-BITS 15)
(def NUM-COUNT-BITS 11)

(def input (slurp (io/resource "day16.txt")))

(def hex-bin-map
  (zipmap "0123456789ABCDEF"
          ["0000" "0001" "0010" "0011"
           "0100" "0101" "0110" "0111"
           "1000" "1001" "1010" "1011"
           "1100" "1101" "1110" "1111"]))

(defn hex->bin [str] (mapcat hex-bin-map str))

(defn to-string [v]
  (if (seq? v) (str/join v) (str v)))

(defn to-decimal [binary]
  (Long/parseLong (to-string binary) 2))

(defn process
  ([packet n f]
   [(f (take n packet)) (drop n packet)])
  ([packet n]
   (process packet n identity)))

(defn drop-header [packet]
  (let [all-binary packet]
    (drop HEADER-LENGTH all-binary)))

(defn read-header [packet]
  (let [header (take HEADER-LENGTH packet)
        [version rest] (process header VERSION-LENGTH to-decimal)
        [type-id _] (process rest TYPE-ID-LENGTH to-decimal)]
    [version type-id]))

(defn add-versions [packet]
  (+ (:version packet) (reduce + (map add-versions (:children packet)))))

(declare parse-packet)
(declare parse-packets-by-length)
(declare parse-packets-by-count)

(defn parse-literal [version type-id packet group-length]
  (let [packet-binary (drop HEADER-LENGTH packet)
        groups (->> packet-binary
                    (partition group-length group-length)
                    (take-upto (fn [part] (= (first part) \0))))
        result (->> groups
                    (map rest)
                    flatten
                    to-decimal)
        groups-length (reduce + (map count groups))
        consumed-length (+ groups-length HEADER-LENGTH)
        remaining (drop consumed-length packet)]
    {:version  version
     :type     :literal
     :type-id  type-id
     :value    result
     :consumed consumed-length
     :next     remaining
     :children '()}))

(defn parse-operator [version type-id packet]
  (let [binary (drop-header packet)
        [length-type-id rest] (process binary 1 first)
        length-length (case length-type-id \0 NUM-LEN-BITS \1 NUM-COUNT-BITS)
        [length rest] (process rest length-length to-decimal)
        children (case length-type-id
                   \0 (parse-packets-by-length rest length)
                   \1 (parse-packets-by-count rest length))
        consumed (+ HEADER-LENGTH 1 length-length (reduce + (map :consumed children)))]

    {:version        version
     :type           :operator
     :type-id        type-id
     :value          type-id
     :consumed       consumed
     :length-type-id length-type-id
     :length         length
     :children       children
     :next           (drop consumed packet)}))

(defn parse-packets-by-count [bits total-count]
  (loop [packets '()
         bits bits]
    (cond
      (= (count packets) total-count) packets
      :else (let [[version type-id] (read-header bits)]
              (case type-id
                4 (let [packet (parse-literal version type-id bits BIT-GROUP-SIZE)]
                    (recur (conj packets packet) (:next packet)))
                (let [packet (parse-operator version type-id bits)]
                  (recur (conj packets packet) (:next packet))))))))

(defn parse-packets-by-length [bits total-length]
  (loop [packets '()
         bits bits
         consumed 0]
    (cond
      (= consumed total-length) packets
      (empty? bits) packets
      :else (let [[version type-id] (read-header bits)]
              (case type-id
                4 (let [packet (parse-literal version type-id bits BIT-GROUP-SIZE)]
                    (recur (conj packets packet) (:next packet) (+ consumed (:consumed packet))))
                ; default
                (let [packet (parse-operator version type-id bits)]
                  (recur (conj packets packet) (:next packet) (+ consumed (:consumed packet)))))))))


(defn parse-packet [packet]
  (let [[version type-id] (read-header packet)]
    (cond
      (= type-id 4) (parse-literal version type-id packet BIT-GROUP-SIZE)
      :else (parse-operator version type-id packet))))

(defn add-versions [packet]
  (+ (:version packet) (reduce + (map add-versions (:children packet)))))

(defn evaluate-packet [packet]
  (case (:type-id packet)
    4 (:value packet)
    0 (reduce + (map evaluate-packet (:children packet)))
    1 (reduce * (map evaluate-packet (:children packet)))
    2 (reduce min (map evaluate-packet (:children packet)))
    3 (reduce max (map evaluate-packet (:children packet)))
    5 (if (reduce < (map evaluate-packet (:children packet))) 1 0)
    6 (if (reduce > (map evaluate-packet (:children packet))) 1 0)
    7 (if (reduce = (map evaluate-packet (:children packet))) 1 0)))

(-> input
    (hex->bin)
    (parse-packet)
    (add-versions))

(-> input
    (hex->bin)
    (parse-packet)
    (evaluate-packet))
