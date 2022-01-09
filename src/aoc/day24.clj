(ns aoc.day24
  (:require [aoc.shared :refer [read-lines]]))

(def alu-steps
  (->> (read-lines "day24.txt")
       (partition 18)
       (apply map vector)
       (remove #(= (count (set %)) 1))
       (apply map vector)
       (map (fn [block]
              (map #(read-string (re-find #"-?\d+" %))
                   block)))))

(defn alu-func
  "Given a z-value, an ALU program block represented by its three unique values
  a, b, and c, and an input value w, returns the resulting value z'."
  [z [a b c] w]
  (let [x  (if (= w (+ (mod z 26) b)) 0 1)  ; x is 0 if w = z mod 26 + b else 1
        y1 (inc (* 25 x))                   ; Z // A * (25x + 1) + x(w + c)
        y2 (* x (+ w c))]
    (+ y2 (quot (* z y1) a))))

(defn inverse-alu-func
  "Given a resulting value z' and an ALU program block, returns a map
  associating each seed-state z to the max/min (`mode`) w value that produces
  that result."
  [z' [a b c] mode]
  (apply merge-with mode
         (for [w (range 1 10)
          ;; match/not-match refers to the condition w = z mod 26 + b
          ;; the additional element % is to reverse the quotient function
               :let [match     (map #(+ % (* a z')) (range a))
                     not-match (map #(+ % (quot (* a (- z' (+ w c)))
                                                26))
                                    (range a))]
               z (set (filter pos? (concat match not-match)))
          ;; Prune the results by checking them forwards
               :when (= z' (alu-func z [a b c] w))]
           {z w})))

(defn table
  "Creates a table with the information necessary to find the max/min (`mode`)
  MONAD number possible step-by-step. If you are at digit D and the current
  z value is Z, (get-in table [D Z]) gives you the next w value you should input."
  [mode]
  (loop [digit 13, z's [0], result {}]
    (if (neg? digit) result
        (let [solutions (apply merge-with mode
                               (for [z' z's]
                                 (inverse-alu-func z' (nth alu-steps digit) mode)))]
          (recur (dec digit) (keys solutions) (conj result {digit solutions}))))))

(defn solve
  "Finds the max/min (`mode`) possible 14-digit MONAD number."
  [mode]
  (let [the-table (table mode)]
    (apply mode
           (remove nil?
                   (for [seed (range 1 10)]
                     (loop [[step & steps] alu-steps
                            result         [seed]
                            z              0]
                       (let [z' (alu-func z step (peek result))]
                         (if steps
                           (when-let [w (get-in the-table [(count result) z'])]
                             (recur steps (conj result w) z'))
                           (read-string (apply str result))))))))))

;Part 1
(solve max)

;Part 2
(solve min)