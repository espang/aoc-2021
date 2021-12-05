(ns aoc.day5
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

(defn parse-line [l]
  (let [parse-cords (fn [xy]
                      (->> (str/split xy #",")
                           (mapv parse-long)))]
    (->> (str/split l #" -> ")
         (mapv parse-cords))))

(defn points-from-line [[[x1 y1] [x2 y2]]]
  (let [step-x (cond
                 (< x1 x2) 1
                 (> x1 x2) -1
                 (= x1 x2) 0)
        step-y (cond
                 (< y1 y2) 1
                 (> y1 y2) -1
                 (= y1 y2) 0)]
    (loop [x x1
           y y1
           points []]
      (if (and (= x x2) (= y y2))
        (conj points [x y])
        (recur (+ x step-x)
               (+ y step-y)
               (conj points [x y]))))))

(defn counter [lines]
  (reduce (fn [counter line]
            (let [points (into #{} (points-from-line line))]
              (reduce (fn [counter point]
                        (update counter point (fnil inc 0)))
                      counter
                      points)))
          {}
          lines))

(defn line-pred [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(comment
  (-> (parse-line "0,9 -> 5,9")
      points-from-line)
  ;; part1
  (->> (read-input-lines 5 parse-line)
       (filter line-pred)
       counter
       vals
       (filter #(> % 1))
       count)
  ;; part2
  (->> (read-input-lines 5 parse-line)
       counter
       vals
       (filter #(> % 1))
       count))
