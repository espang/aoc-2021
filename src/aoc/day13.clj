(ns aoc.day13
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

(defn parse-line [l]
  (mapv parse-long (str/split l #",")))

(defn fold-x [x-fold coords]
  (into #{} (for [[x y] coords]
              (if (> x x-fold)
                [(- x-fold (- x x-fold)) y]
                [x y]))))

(defn fold-y [y-fold coords]
  (into #{} (for [[x y] coords]
              (if (> y y-fold)
                [x (- y-fold (- y y-fold))]
                [x y]))))

(defn make-row [y max-x coords]
  (apply str (for [x (range (inc max-x))]
               (if (coords [x y])
                 "#"
                 " "))))

(defn print [coords]
  (let [max-x (apply max (map first coords))
        max-y (apply max (map second coords))]
    (doseq [y (range (inc max-y))]
      (println (make-row y max-x coords)))))

(comment
  (parse-line "-1,-2")
  ;; part1
  (-> (fold-y 7 (read-input-lines 13 parse-line true))
      count)
  (->> (fold-x 655 (read-input-lines 13 parse-line))
      count)

  ;; part2
  (->> (read-input-lines 13 parse-line true)
       (fold-y 7)
       (fold-x 5)
       print)
  (->> (read-input-lines 13 parse-line)
       (fold-x 655)
       (fold-y 447)
       (fold-x 327)
       (fold-y 223)
       (fold-x 163)
       (fold-y 111)
       (fold-x 81)
       (fold-y 55)
       (fold-x 40)
       (fold-y 27)
       (fold-y 13)
       (fold-y 6)
       print)
  )