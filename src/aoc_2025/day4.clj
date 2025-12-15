(ns aoc-2025.day4
  (:require [clojure.string :as s]))

(def input (slurp "src/aoc_2025/inputs/day4.txt"))

(defn accessible? [coll [i j]]
  (let [h (count coll)
        w (count (first coll))
        indices (filter #(and
                          (not (neg? (first %)))
                          (< (first %) w)
                          (not (neg? (second %)))
                          (< (second %) h))
                        (for [di [-1 0 1]
                              dj [-1 0 1]
                              :when (not= [di dj] [0 0])]
                          [(+ i di) (+ j dj)]))
        neighbors (filter #(= (get-in coll %) \@) indices)]
    (< (count neighbors) 4)))

(defn solve-1 [inp]
  (let [lines (s/split-lines inp)
        h (count lines)
        w (count (first lines))]
    (count
     (filter
      #(accessible? lines %)
      (for [i (range w)
            j (range h)
            :when (= (get-in lines [i j]) \@)]
        [i j])))))

(solve-1 input)
;; => 1376

(defn find-removeable [coll]
  (let [h (count coll)
        w (count (first coll))]
    (filter
     #(accessible? coll %)
     (for [i (range w)
           j (range h)
           :when (= (get-in coll [i j]) \@)]
       [i j]))))

(defn replace-with-dot [coll [i j]]
  (update coll i #(str (subs % 0 j)
                     \.
                     (when (< j (count %))
                       (subs % (inc j))))))

(defn remove-removeable [[coll n]]
  (let [removeable (find-removeable coll)]
    [(reduce replace-with-dot coll removeable) (count removeable)]))

(defn solve-2 [coll]
  (->> (iterate remove-removeable [coll nil])
       (drop 1)
       (take-while #(pos? (second %)))
       (reduce #(+ %1 (second %2)) 0)))

(solve-2 (s/split-lines input))
;; => 8587

(comment
  (def sample-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

  (solve-1 sample-input)

  (accessible? (s/split-lines sample-input) [1 1])
  (find-removeable (s/split-lines sample-input))
  (update (s/split-lines sample-input) 0 #())
  (replace-with-dot (s/split-lines sample-input) [0 2])
  (remove-removeable (remove-removeable [(s/split-lines sample-input) 0]))
  (solve-2 (s/split-lines sample-input))
  )
