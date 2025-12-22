(ns aoc-2025.day9
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "src/aoc_2025/inputs/day9.txt"))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(str/split % #","))
       (map (fn [[a b]] [(Integer/parseInt a) (Integer/parseInt b)]))))

(defn pairs [coll]
  (for [i (range (count coll))
        j (range (inc i) (count coll))]
    [(nth coll i) (nth coll j)]))

(defn bounded-area [[[a b] [a' b']]]
  (abs (* (inc (- a a')) (inc (- b b')))))

(defn solve-1 [inp]
  (apply max (map bounded-area (pairs (parse-input inp)))))

(solve-1 input)
;; => 4761736832

(defn index-of-with [pred v]
  (->> v
       (map-indexed #(list %1 %2))
       (drop-while #(not (pred (second %))))
       first
       first))

(defn insert-at [v n x]
  (into [] (concat (conj (subvec v 0 n) x) (subvec v n))))

(defn remove-nth [v n]
  (into [] (concat (subvec v 0 n) (subvec v (inc n)))))

(defn scan-column [n red-tiles previous-column]
  (when (zero? (mod n 10000))
    (println (format "On column %d" n)))
  (let [red-tiles-in-column (filter #(= n (first %)) red-tiles)
        tile-intervals (->> red-tiles-in-column
                            (map second)
                            (partition 2)
                            (map sort)
                            (map vec)
                            sort)
        clean-intervals (fn [intervals i]
                          (if (and (not (neg? i)) (< i (dec (count intervals))))
                            (let [[x y] (intervals i)
                                  [x' y'] (intervals (inc i))]
                              (if (= y x')
                                (-> intervals
                                    (assoc i [x y'])
                                    (remove-nth (inc i)))
                                intervals))
                            intervals))]
   (reduce (fn [[next-column to-remove] [a b]]
              (let [i (index-of-with #(<= b (second %)) next-column)]
                (if-let [[a' b'] (when i (next-column i))]
                  (cond
                    (= b a') [(clean-intervals (assoc next-column i [a b']) (dec i)) to-remove]
                    (= a b') [(clean-intervals (assoc next-column i [a' b]) i) to-remove]
                    (and (>= a a') (<= b b')) [next-column (conj to-remove [a b])]
                    (< b a') [(clean-intervals
                               (into [] (-> next-column
                                            (subvec 0 i)
                                            (conj [a b])
                                            (concat (subvec next-column i))))
                               (dec i))
                              to-remove]
                    :else (throw (IllegalStateException. (format "Oh no! last: %s, new: %s" [a' b'] [a b]))))
                  [(clean-intervals (conj next-column [a b]) (dec (count next-column)))])))
            [previous-column ()]
            tile-intervals)))

(defn process-tiles [red-tiles]
  (let [min-x (apply min (map first red-tiles))
        min-y (apply min (map second red-tiles))]
    (map (fn [[a b]] [(- a min-x) (- b min-y)]) red-tiles)))

(defn build-region [red-tiles]
  (let [min-x (apply min (map first red-tiles))
        max-x (apply max (map first red-tiles))
        min-y (apply min (map second red-tiles))
        red-tiles (map (fn [[x y]] [(- x min-x) (- y min-y)]) red-tiles)]
    (first
     (reduce (fn [[columns to-remove] n]
               (let [previous-column (reduce
                         (fn [column [c d]]
                           (let [j (index-of-with #(<= d (second %)) column)
                                 [x y] (column j)]
                             (cond
                               (and (= c x) (= d y)) (remove-nth column j)
                               (= c x) (assoc column j [d y])
                               (= d y) (assoc column j [x c])
                               (and (> c x) (< d y)) (-> column
                                                         (assoc j [x c])
                                                         (insert-at (inc j) [d y]))
                               :else (throw (IllegalStateException. (format "Bad removing! column: %s, removing: %s" column [c d]))))))
                         (last columns)
                         to-remove)
                     [next-column next-to-remove] (scan-column n red-tiles previous-column)]
                 [(conj columns next-column) next-to-remove]))
             [[(->> red-tiles
                    (filter #(zero? (first %)))
                    (map second)
                    (partition 2 1)
                    (map sort)
                    (map vec)
                    sort
                    vec)]
              ()]
             (range 1 (inc (- max-x min-x)))))))

(defn contains-interval? [[a b] intervals]
  (reduce (fn [_ [a' b']]
            (cond
              (and (>= a a') (<= b b')) (reduced true)
              (or (>= b' a) (<= a b')) (reduced false)
              :else false))
          false
          intervals))

(defn solve-2 [red-tiles]
  (let [region (build-region red-tiles)]
    (println "Built region")
    (reduce (fn [result [[a b] [a' b']]]
              (if (every? #(contains-interval? [(min b b') (max b b')] %)
                          (subvec region (min a a') (min (inc (max a a')) (dec (count region)))))
                (max result (* (inc (abs (- a a'))) (inc (abs (- b b')))))
                result))
            0
            (pairs (process-tiles red-tiles)))))

(defn in-interval? [x [a b]]
  (and (> x (min a b)) (< x (max a b))))

(solve-2 (parse-input input))
;; => 1452422268

(comment
  (def sample-input "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

  (def sample-pairs (pairs (parse-input sample-input)))

  (apply max (map bounded-area sample-pairs))
  (find-region (parse-input sample-input))
  (apply max (map first (parse-input input)))

  (def sample-red-tiles (map (fn [[a b]] [(- a 2) (- b 1)]) (parse-input sample-input)))
  (.indexOf sample-red-tiles [7 6])
  (sort-by first '((1 2) (0 4) (7 8)))
  (build-region (parse-input sample-input))
  (solve-2 (parse-input sample-input))
  (contains-interval? [3 5] [[0 1] [2 6]])
  (some (fn [[[[a b] [a' b']] [[c d] [c' d']]]]
          (cond
            (and (= a a') (= d d')) (and (in-interval? a [c c'])
                                         (in-interval? d [b b']))
            (and (= b b') (= c c')) (and (in-interval? b [d d'])
                                         (in-interval? c [a a']))))
        (pairs (partition 2 1 (parse-input input))))
  )
