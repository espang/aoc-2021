(ns aoc.day11
  (:require
   [clojure.set :as set]))

(def test-input ["5483143223"
                 "2745854711"
                 "5264556173"
                 "6141336146"
                 "6357385478"
                 "4167524645"
                 "2176841721"
                 "6882881134"
                 "4846848554"
                 "5283751526"])

(def real-input ["4525436417"
                 "1851242553"
                 "5421435521"
                 "8431325447"
                 "4517438332"
                 "3521262111"
                 "3331541734"
                 "4351836641"
                 "2753881442"
                 "7717616863"])

(defn into-mat [input]
  (mapv (fn [l] (mapv (comp parse-long str) l)) input))

(defn increase-by-one [matrix]
  (mapv #(mapv inc %) matrix))

(defn inc-at [matrix pos]
  (update-in matrix pos inc))

(defn zero-at [matrix pos]
  (assoc-in matrix pos 0))

(def in-mat? (fn [[row col]] (and (<= 0 row 9)
                                  (<= 0 col 9))))

(defn neighbours-of [[row col]]
  (->> [[-1 -1] [0 -1] [1 -1]
        [-1 0] [0 0] [1 0]
        [-1 1] [0 1] [1 1]]
       (map (fn [[r c]] [(+ row r) (+ col c)]))
       (filter in-mat?)))

(defn flash [matrix]
  (loop [handled #{}
         m       (increase-by-one matrix)]
    (let [flashers (set/difference
                    (into #{} (for [r (range 10)
                                    c (range 10)
                                    :when (> ((m r) c) 9)]
                                [r c]))
                    handled)
          neighbours (mapcat neighbours-of flashers)
          m' (reduce inc-at m neighbours)]
      (if (zero? (count flashers))
        [(reduce zero-at m handled)
         (count handled)]
        (recur (set/union handled flashers)
               m')))))

(defn flash-n [n matrix]
  (loop [n n
         m matrix
         c 0]
    (if (zero? n)
      c
      (let [[m' c'] (flash m)]
        (recur (dec n)
               m'
               (+ c c'))))))

(comment
  (into-mat test-input)
  (def resp (flash (into-mat test-input)))
  (flash (first resp))
  (flash-n 10 (into-mat test-input))
  (flash-n 100 (into-mat test-input))
  (flash-n 100 (into-mat real-input)))

(defn all-flash [matrix]
  (loop [step 1
         m matrix]
    (let [[m' n] (flash m)]
      (if (= n 100)
        step
        (recur (inc step)
               m')))))

(comment
  (all-flash (into-mat test-input))
  (all-flash (into-mat real-input)))