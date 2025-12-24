(ns aoc-2025.day11
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(def input (slurp "src/aoc_2025/inputs/day11.txt"))

(defn parse-input [s]
  (->> (str/split-lines s)
       (map #(str/split % #": "))
       (mapcat #(list (first %) (set (str/split (second %) #"\s"))))
       (apply hash-map)))

(defn dfs [m node]
  (let [outputs (get m node)]
    (if (contains? outputs "out")
      1
      (apply + (for [node outputs] (dfs m node))))))

(dfs (parse-input input) "you")
;; => 670

(defn dfs-2 [m & {:keys [node dac? fft?] :or {node "svr" dac? false fft? false}}]
  (let [outputs (get m node)]
    (if (and (= node "out") dac? fft?)
      1
      (reduce + (for [next-node outputs]
                  (dfs-2' m
                          :node next-node
                          :dac? (or dac? (= node "dac"))
                          :fft? (or fft? (= node "fft"))))))))

(def dfs-2' (memoize dfs-2))

(dfs-2' (parse-input input))
;; => 332052564714990

(filter #(contains? (second %) "out") (parse-input input))
(comment
  (def sample-input "aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out")

  (def sample-input-2 "svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out")

  (parse-input sample-input)

  (dfs (parse-input sample-input) "you")
  (dfs-2' (parse-input sample-input-2))
  )
