(ns aoc-2025.day1
  (:require [clojure.string :as s]
            [clojure.math :as math]))

(def input (slurp "src/aoc_2025/inputs/day1.txt"))

(defn parse-line [s]
  (let [n (Integer/parseInt (subs s 1))]
    (if (= (first s) \R)
      n
      (* -1 n))))

(defn parse-input [s]
  (map parse-line (s/split-lines s)))

(defn process-1 [ns]
  (reduce (fn [[ct dial] n]
            (let [new-dial (mod (+ dial n) 100)]
              (if (zero? new-dial)
                [(inc ct) new-dial]
                [ct new-dial])))
          [0 50]
          ns))

(process-1 (parse-input input))

(defn process-2 [ns]
  (reduce (fn [[ct dial] n]
            (let [dial-sum (+ dial n)
                  new-dial (mod dial-sum 100)
                  passed-zero (let [div (math/floor-div dial-sum 100)]
                                (if (zero? dial)
                                  (if (neg? div)
                                    (dec (abs div))
                                    div)
                                  (if (zero? dial-sum)
                                    1
                                    (if (zero? new-dial)
                                      (if (neg? div)
                                        (inc (abs div))
                                        div)
                                      (abs div)))))]
              [(+ passed-zero ct) new-dial]))
          [0 50]
          ns))

(process-2 (parse-input input))

(comment
  (def sample-input "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

  (parse-line "R30")
  (parse-input sample-input)
  (process-2 (parse-input sample-input))
  (math/floor-div 301 100)
  )
