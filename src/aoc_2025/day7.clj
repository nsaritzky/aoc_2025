(ns aoc-2025.day7
  (:require
   [clojure.string :as s]))

(def input (slurp "src/aoc_2025/inputs/day7.txt"))

(defn split-beams [beams row]
  (->> beams
       (mapcat #(if (= \^ (nth row %))
                  [(dec %) (inc %)]
                  [%]))
       distinct))

(defn solve-1 [inp]
  (let [lines (s/split-lines inp)]
    (second
     (reduce (fn [[beams res] row]
               [(split-beams beams row) (+ res (count (filter #(= \^ (nth row %)) beams)))])
             [[(s/index-of (first lines) "S")] 0]
             (rest lines)))))

(solve-1 input)
;; => 1649

(defn quantum-split-beams [beams row]
  (reduce (fn [new-beams i]
            (let [n (nth beams i)]
              (if (= \^ (nth row i))
                (-> new-beams
                    (update (dec i) #(+ % n))
                    (update (inc i) #(+ % n)))
                (update new-beams i #(+ % n)))))
          (vec (repeat (count row) 0))
          (range (count beams))))

(defn solve-2 [inp]
  (let [lines (s/split-lines inp)
        start-beams (assoc (vec (repeat (count (first lines)) 0))
                           (s/index-of (first lines) "S")
                           1)]
    (->> (rest lines)
         (reduce quantum-split-beams start-beams)
         (reduce +))))

(solve-2 input)
;; => 16937871060075

(comment
  (def sample-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")
  (some #(re-find #"\^\^" %) (s/split-lines input))
  (let [lines (s/split-lines sample-input)]
    (split-beams [(s/index-of (first lines) "S")] (nth lines 2)))
  (solve-1 sample-input)
  (let [lines (s/split-lines sample-input)]
    (quantum-split-beams
     (assoc
      (vec (repeat (count (first lines)) 0))
      (s/index-of (first lines) "S")
      1)
     (nth lines 2)))
  (solve-2 sample-input)
  )
