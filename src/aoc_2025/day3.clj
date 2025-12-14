(ns aoc-2025.day3
  (:require [clojure.string :as s]))

(def input (slurp "src/aoc_2025/inputs/day3.txt"))

(defn parse-input [s]
  (->> (s/split-lines s)
       (map #(s/split % #""))
       (map (fn [chars] (map #(Integer/parseInt (str %)) chars)))))

(defn max-joltage [ns]
  (let [d1 (apply max (butlast ns))
        i (.indexOf ns d1)
        d2 (apply max (drop (inc i) ns))]
    (+ (* 10 d1) d2)))

(reduce + (map max-joltage (parse-input input)))
;; => 17376

(defn get-next-digit [k ns]
  (let [d (apply max (take (- (count ns) k) ns))
        i (.indexOf ns d)]
    [d i]))

(defn collect [[ns s]]
  (let [[d i] (get-next-digit (- 11 (count s)) ns)]
    [(drop (inc i) ns) (str s d)]))

(defn max-joltage-2 [ns]
  (Long/parseLong (second (nth (iterate collect [ns ""]) 12))))

(reduce + (map max-joltage-2 (parse-input input)))
;; => 172119830406258

(comment
  (def sample-input "987654321111111
811111111111119
234234234234278
818181911112111")

  (map max-joltage (parse-input sample-input))

  (parse-input sample-input)
  (.indexOf '(1 2 3) 3)
  (collect '(7 8 9 6) "")
  (get-next-digit 2 '(7 8 9 6))
  (map max-joltage-2 (parse-input sample-input))
  (reduce + (map max-joltage-2 (parse-input sample-input)))
  )
