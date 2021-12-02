(ns aoc.day2
  (:require
   [aoc.helpers :refer [read-input-lines]]))

(defn parse-line [line]
  (let [[direction val] (str/split line #" ")]
    [(case direction
       "forward" :forward
       "down" :down
       "up" :up)
     (Integer/parseInt val)]))

;; part 1
(defn move-step [[pos depth] [dir val]]
  (case dir
    forward [(+ pos val) depth]
    down [pos (+ depth val)]
    up [pos (- depth val)]))

(->> "input_2.txt"
     read-input
     (reduce move-step [0 0])
     (apply *))

;; part 2
(defn move-step-2 [[pos aim depth] [dir val]]
  (case dir
    :forward [(+ pos val) aim (+ depth (* aim val))]
    :down    [pos (+ aim val) depth]
    :up      [pos (- aim val) depth]))

(defn score [[pos _ depth]] (* pos depth))

(->> "input_2.txt"
     read-input
     (reduce move-step-2 [0 0 0])
     score)
