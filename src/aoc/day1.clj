(ns aoc.day1
  (:require
   [aoc.helpers :refer [read-input-lines]]))

;; part 1
(->> (read-input-lines 1 parse-long)
     (partition 2 1)
     (map (partial apply -))
     (filter neg?)
     count)

;; part 2
(->> (read-input-lines 1 parse-long)
     (partition 3 1)
     (map (partial reduce +))
     (partition 2 1)
     (map (partial apply -))
     (filter neg?)
     count)