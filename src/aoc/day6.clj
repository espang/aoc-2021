(ns aoc.day6)

(def test-input [3 4 3 1 2])

(def input [4,1,1,1,5,1,3,1,5,3,4,3,3,1,3,3,1,5,3,2,4,4,3,4,1,4,2,2,1,3,5,1,1,3,2,5,1,1,4,2,5,4,3,2,5,3,3,4,5,4,3,5,4,2,5,5,2,2,2,3,5,5,4,2,1,1,5,1,4,3,2,2,1,2,1,5,3,3,3,5,1,5,4,2,2,2,1,4,2,5,2,3,3,2,3,4,4,1,4,4,3,1,1,1,1,1,4,4,5,4,2,5,1,5,4,4,5,2,3,5,4,1,4,5,2,1,1,2,5,4,5,5,1,1,1,1,1,4,5,3,1,3,4,3,3,1,5,4,2,1,4,4,4,1,1,3,1,3,5,3,1,4,5,3,5,1,1,2,2,4,4,1,4,1,3,1,1,3,1,3,3,5,4,2,1,1,2,1,2,3,3,5,4,1,1,2,1,2,5,3,1,5,4,3,1,5,2,3,4,4,3,1,1,1,2,1,1,2,1,5,4,2,2,1,4,3,1,1,1,1,3,1,5,2,4,1,3,2,3,4,3,4,2,1,2,1,2,4,2,1,5,2,2,5,5,1,1,2,3,1,1,1,3,5,1,3,5,1,3,3,2,4,5,5,3,1,4,1,5,2,4,5,5,5,2,4,2,2,5,2,4,1,3,2,1,1,4,4,1,5
])

(defn steps [n initial]
  (loop [n  n
         xs initial]
    (if (zero? n)
      xs
      (let [[xs' c] (reduce (fn [[acc c] itm]
                              (if (zero? itm)
                                [(conj acc 6) (inc c)]
                                [(conj acc (dec itm)) c]))
                            [[] 0]
                            xs)]
        (recur (dec n) (into xs' (repeat c 8)))))))

(comment
  (count (steps 18 test-input))
  (time (count (steps 80 input))))

;; part2
(def steps-64 (into {} (for [x (range 9)]
                         [x (count (steps 64 [x]))])))
(def steps-128 (into {} (for [x (range 9)]
                          [x (->> (steps 64 [x])
                                  (map steps-64)
                                  (reduce +))])))
(def steps-192 (into {} (for [x (range 9)]
                          [x (->> (steps 64 [x])
                                  (map steps-128)
                                  (reduce +))])))
(def steps-256 (into {} (for [x (range 9)]
                          [x (->> (steps 64 [x])
                                  (map steps-192)
                                  (reduce +))])))
(time
 (->> input
      (map steps-256)
      (reduce +)))