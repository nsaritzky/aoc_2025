(ns aoc-2025.day10
  (:require
   [clojure.string :as str]))

(def input (slurp "src/aoc_2025/inputs/day10.txt"))

(defn parse-line [s]
  (let [[goal-str button-str] (str/split s #"\s" 2)
        goal (mapv #(= % \#) (subs goal-str 1 (- (count goal-str) 1)))
        buttons (->> (str/split button-str #"\s")
                     butlast
                     (map #(subs % 1 (- (count %) 1)))
                     (map #(str/split % #","))
                     (map (fn [toggles] (map #(Integer/parseInt %) toggles))))]
    [goal buttons]))

(defn parse-input [s]
  (->> s
       str/split-lines
       (map parse-line)))

(defn powerset [coll]
  (if (empty? coll)
    '(#{})
    (let [without-first (powerset (rest coll))]
      (concat without-first (map #(conj % (first coll)) without-first)))))

(defn press-button [state toggles]
  (reduce (fn [state toggle] (update state toggle not)) state toggles))

(defn solve-row  [[goal buttons]]
  (->> (powerset (range (count buttons)))
       (sort-by count)
       (map #(list (reduce (fn [state i] (press-button state (nth buttons i))) goal %) (count %)))
       (filter #(every? false? (first %)))
       first
       second))

(defn solve-1 [rows]
  (->> rows
       (map solve-row)
       (reduce +)))

(solve-1 (parse-input input))
;; => 520

(defn parse-row-2 [s]
  (let [joltages (map #(Integer/parseInt %)
                      (str/split (second (re-find #"\{(\d+(?:,\d+)*)\}" s)) #","))
        buttons (->> s
                     (re-seq #"\((\d+(?:,\d+)*)\)")
                     (map second)
                     (map #(str/split % #","))
                     (map #(map (fn [s] (Integer/parseInt s)) %)))]
    [buttons joltages]))

(defn parse-input-2 [s]
  (->> s
       (str/split-lines)
       (map parse-row-2)))

(defn k* [k v]
  (mapv #(* k %) v))

(defn +v [a b & more]
  (if (seq more)
    (apply +v (+v a b) more)
    (mapv + a b)))

(defn comb [k v & more]
  (let [next (k* k v)]
   (if (empty? more)
     next
     (+v next (apply comb more)))))

(defn powerset-n [n]
  (powerset (range n)))

(def powerset-n' (memoize powerset-n))

(defn press-button-2 [state toggles]
  (reduce #(update %1 %2 dec) state toggles))

(defn match-parity [[buttons target]]
  (let [target (vec target)]
   (->> (powerset-n' (count buttons))
        (map #(list (reduce (fn [state i] (press-button-2 state (nth buttons i))) target %) (count %)))
        (filter #(every? (every-pred (complement neg?) even?) (first %))))))

(def match-parity' (memoize match-parity))

(defn solve-row-2 [[buttons target] & {:keys [n] :or {n 0}}]
  (if (every? zero? target)
    n
    (reduce (fn [result [state-after presses]]
              (let [new-target (map #(/ % 2) state-after)]
                (min result (+ n (* 2 (solve-row-2' [buttons new-target] :n presses))))))
            ##Inf
            (match-parity' [buttons target]))))

(def solve-row-2' (memoize solve-row-2))

(defn solve-2 [rows]
  (/ (->> rows
        (map solve-row-2)
        (reduce +))
     2))

(solve-2 (parse-input-2 input))
;; => 20626

(comment
  (def sample-input
    "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

  (parse-input sample-input)
  (parse-input-2 sample-input)

  (solve-1 (parse-input sample-input))
  (solve-2 (parse-input-2 sample-input))

  (solve-row-2' (nth (parse-input-2 sample-input) 2))

  (press-button [true false false true] '(1 3))
  )
