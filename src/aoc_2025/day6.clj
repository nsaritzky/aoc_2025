(ns aoc-2025.day6
  (:require
   [clojure.string :as s]))

(def input (slurp "src/aoc_2025/inputs/day6.txt"))

(defn solve-1 [s]
  (let [lines (s/split-lines s)
        rows (for [line (butlast lines)]
               (map #(Integer/parseInt %) (s/split (s/trim line) #"\s+")))
        operators (s/split (last lines) #"\s+")]
    (reduce +
            (for [i (range (count operators))
                  :let [op (if (= (nth operators i) "*") * +)]]
              (apply op (map #(nth % i) rows))))))

(solve-1 input)
;; => 6725216329103

(defn index-of-regex [re s]
  (when-let [match (re-find re s)]
    (s/index-of s match)))

(defn operator-indices [operator-str]
  (let [res (atom [])
        s (atom operator-str)]
    (while (re-find #"\S" @s)
      (let [i (index-of-regex #"\S" @s)]
        (swap! res #(conj % (+ i (if (empty? %) 0 (+ 1 (last %))))))
        (swap! s #(when (< i (count @s))
                    (subs % (inc i))))))
    @res))

(defn nth-when [coll n]
  (when (< n (count coll))
    (nth coll n)))

(defn split-with-spaces [strs operator-indices]
  (for [s strs]
    (map (fn [[i j]] (subs s i (dec j)))
         (conj
          (vec (partition 2 1 operator-indices))
          (list (last operator-indices) (inc (count (first strs))))))))

(defn cephalapodify [strs]
  (->> (range 0 (count (first strs)))
       (map (fn [i] (apply str (map #(nth % i) strs))))
       (map #(Integer/parseInt (s/trim %)))))

(defn solve-2 [inp]
  (let [lines (s/split-lines inp)
        operator-ixs (operator-indices (last lines))
        operators (map #(nth (last lines) %) operator-ixs)
        rows (split-with-spaces (butlast lines) operator-ixs)
        columns (map (fn [i] (map #(nth % i) rows)) (range (count operators)))
        values (map cephalapodify columns)]
    (->> values
         (map-indexed #(if (= (nth operators %1) \*)
                         (apply * %2)
                         (apply + %2)))
         (reduce +))))

(solve-2 input)
;; => 10600728112865

(comment
  (def sample-input "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +  ")

  (solve-1 sample-input)
  (cephalapodify [64 23 314])
  (solve-2 sample-input)
  (re-find #"\S" "  6")
  (index-of-regex #"\S" "  ")
  (re-find #"\S" "  ")
  (operator-indices (last (s/split-lines sample-input)))
  (nth (last (s/split-lines sample-input)) 12)
  (split-with-spaces (butlast (s/split-lines sample-input)) (operator-indices (last (s/split-lines sample-input))))
  (cephalapodify ["64 " "23 " "314"])
  (solve-2 sample-input)
  )
