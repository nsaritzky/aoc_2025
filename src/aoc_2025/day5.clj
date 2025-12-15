(ns aoc-2025.day5
  (:require [clojure.string :as s]))

(def input (slurp "src/aoc_2025/inputs/day5.txt"))

(defn parse-range [s]
  (let [[a b] (s/split s #"-")]
    [(Long/parseLong a) (Long/parseLong b)]))

(defn parse-input [s]
  (let [[ranges ids] (s/split s #"\n\n")]
    [(map parse-range (s/split-lines ranges))
     (map #(Long/parseLong %) (s/split-lines ids))]))

(defn solve-1 [[ranges ids]]
  (letfn [(check-freshness [id]
           (some #(and (>= id (first %)) (<= id (second %))) ranges))]
    (count (filter check-freshness ids))))

(solve-1 (parse-input input))
;; => 739

(defn overlaps? [[a b] [a' b']]
  (or
   (and (>= b a') (<= b b'))
   (and (>= b' a) (<= b' b))))


(defn merge-range-into [ranges [a b]]
  )

(defn solve-2 [ranges]
  (let [ranges (sort ranges)]
    (reduce (fn [vec [a b]]
              (let [[a' b'] (last vec)]
                (if (<= a b')
                  (assoc vec (dec (count vec)) [a' b])
                  (conj vec [a b])))))))

(solve-2 (first (parse-input input)))

(comment
  (def sample-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

  (solve-1 (parse-input sample-input))
  (solve-2 (first (parse-input sample-input)))
  (overlaps? [1 2] [0 5])
  (merge-range-into [] [5 6])
  )
