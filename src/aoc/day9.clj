(ns aoc.day9
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

(defn parse-line [s]
  (mapv (comp parse-long str) (seq s)))

(defn value-at [[row col] matrix]
  "Returns the value at the cell or 10,
   a value that is larger than any expected value"
  (nth (nth matrix row []) col 10))

(defn low-point [[row col] matrix]
  (let [v (value-at [row col] matrix)
        neighbours (map (fn [[drow dcol]] [(+ row drow) (+ col dcol)])
                        [[-1 0] [1 0] [0 -1] [0 1]])]
    (every? #(< v (value-at % matrix)) neighbours)))

(defn part1 [matrix]
  (let [max-row (count matrix)
        max-col (count (first matrix))]
    (->> (for [row (range max-row)
               col (range max-col)
               :when (low-point [row col] matrix)]
           (value-at [row col] matrix))
         (map inc)
         (reduce +))))

(comment
  (part1 (vec (read-input-lines 9 parse-line true)))
  (part1 (vec (read-input-lines 9 parse-line))))


(defn basin-size [[row col] matrix]
  (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) [row col])
         seen #{}
         count 0]
    (if (empty? q)
      count
      (let [e         (peek q)
            [row col] e
            neighbours (->> [[-1 0] [1 0] [0 -1] [0 1]]
                            (map (fn [[drow dcol]] [(+ row drow) (+ col dcol)]))
                            (filter #(> 9 (value-at % matrix))))]
        (if (contains? seen e)
          (recur (pop q) seen count)
          (recur (apply conj (pop q) neighbours)
                 (conj seen e)
                 (inc count)))))))

(comment 
  (basin-size [0 9] (vec (read-input-lines 9 parse-line true))))

(defn part2 [matrix]
  (let [max-row (count matrix)
        max-col (count (first matrix))]
    (->> (for [row (range max-row)
               col (range max-col)
               :when (low-point [row col] matrix)]
           [row col])
         (map (fn [coord] (basin-size coord matrix)))
         (sort)
         (reverse)
         (take 3)
         (reduce * 1))))

(comment
  (part2 (vec (read-input-lines 9 parse-line true)))
  (part2 (vec (read-input-lines 9 parse-line))))

