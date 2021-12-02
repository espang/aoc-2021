(ns aoc.day2
  (:require
   [clojure.string :as str]))

(defn parse-line [line]
  (let [[direction val] (str/split line #" ")]
    [(case direction
       "forward" :forward
       "down" :down
       "up" :up)
     (Integer/parseInt val)]))

(defn read-input [filename]
  (->> filename
       slurp
       str/split-lines
       (map parse-line)))

(comment
  (read-input "input_2_test.txt"))

;; part 1
(defn move-step [[pos depth] [dir val]]
  (case dir
    :forward [(+ pos val) depth]
    :down [pos (+ depth val)]
    :up [pos (- depth val)]))
]
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
