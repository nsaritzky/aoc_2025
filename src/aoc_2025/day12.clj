(ns aoc-2025.day12
  (:require
   [clojure.string :as str]))

(def input (slurp "src/aoc_2025/inputs/day12.txt"))

(defn parse-region [s]
  (let [[size-str v-str] (str/split s #": ")
        size (map #(Integer/parseInt %) (str/split size-str #"x"))
        v (map #(Integer/parseInt %) (str/split v-str #"\s"))]
    [size v]))

(defn parse-input [s]
  (let [blocks (str/split s #"\n\n")
        presents (map #(second (str/split % #"\s" 2)) (butlast blocks))
        regions (str/split-lines (last blocks))]
    [presents (map parse-region regions)]))

(defn solve-1?? [[presents regions]]
  (let [present-sizes (->> presents
                           (map #(re-seq #"#" %))
                           (map count))]
    (->> regions
         (map (fn [[[a b] v]] [(* a b) v]))
         (map (fn [[area v]] [area (->> v
                                        (map-indexed #(* (nth present-sizes %1) %2))
                                        (reduce +))]))
         (filter (fn [[area total]] (>= area total)))
         count)))

(solve-1?? (parse-input input))

(comment
  (def sample-input "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

  (str/split sample-input #"\n\n")
  (solve-1?? (parse-input sample-input))
  )
