(ns aoc.day1
  (:require
   [clojure.string :as str]))

(defn read-input
  [filename]
  (->> (slurp filename)
       str/split-lines
       (map #(Integer/parseInt %))))

;; part 1
(defn changes [coll]
  (->> coll
       (partition 2 1)
       (map (fn [[v1 v2]] (- v2 v1)))))

(->> "input_1.txt"
    read-input
    changes
    (filter pos?)
     count)

;; part 2
(defn sliding-window-3 [coll]
  (->> coll
       (partition 3 1)
       (map #(reduce + %))))

(comment
  (sliding-window-3 [1 2 3 4 5]))

(->> "input_1.txt"
     read-input
     sliding-window-3
     changes
     (filter pos?)
     count)