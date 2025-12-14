(ns aoc-2025.day2
  (:require [clojure.string :as s]))

(def input (s/trim (slurp "src/aoc_2025/day2.txt")))

(defn parse-ranges [s]
  (->> (s/split s #",")
       (map #(s/split % #"-"))
       (map #(vector (Long/parseLong (first %)) (Long/parseLong (second %))))))

(defn invalid? [n]
  (let [s (str n)
        l (count s)]
    (and (even? l)
         (= (subs s 0 (/ l 2)) (subs s (/ l 2))))))

(defn find-invalid-in-range [[a b]]
  (filter invalid? (range a (inc b))))

(defn process-1 [ranges]
  (reduce + (mapcat find-invalid-in-range ranges)))

(process-1 (parse-ranges input))
;; => 26255179562

(defn invalid-2? [n]
  (let [s (str n)
        l (count s)]
    (and (> l 1)
         (some
          (fn [ps] (every? #(= (first ps) %) (rest ps)))
          (->> (range 1 (inc (/ l 2)))
               (filter #(zero? (mod l %)))
               (map #(partition % s)))))))

(defn find-invalid-in-range-2 [[a b]]
  (filter invalid-2? (range a (inc b))))

(defn process-2 [ranges]
  (reduce + (mapcat find-invalid-in-range-2 ranges)))

(process-2 (parse-ranges input))
;; => 31680313976 ✅
;; => 31680314021

(comment
  (def sample-input (str "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
                     "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
                     "824824821-824824827,2121212118-2121212124"))

  (parse-ranges sample-input)
  (s/split sample-input #",")
  (str 122)
  (invalid? 121234)
  (find-invalid-in-range [95 115])
  (reduce + (mapcat find-invalid-in-range (parse-ranges sample-input)))
  (invalid-2? 112)
  (process-2 (parse-ranges sample-input))
  (find-invalid-in-range-2 [95 115])
  (partition 1)

  (doseq [n (mapcat find-invalid-in-range-2 (parse-ranges input))] (println n))
  )
