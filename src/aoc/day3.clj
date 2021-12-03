(ns aoc.day3
  (:require
   [aoc.helpers :refer [read-input-lines]]))

(defn transpose [xs]
  (apply map list xs))

(defn part1 []
  (let [[gamma epsilon] (->> (read-input-lines 3 identity)
                             transpose
                             (map frequencies)
                             (reduce (fn [[gamma epsilon] m]
                                       (let [count-1 (m \1)
                                             count-0 (m \0)]
                                         (if (> count-1 count-0)
                                           [(conj gamma \1)
                                            (conj epsilon \0)]
                                           [(conj gamma \0)
                                            (conj epsilon \1)])))
                                     [[] []]))]
    (* (Integer/parseInt (apply str gamma) 2)
       (Integer/parseInt (apply str epsilon) 2))))

;; part 2
(defn oxygen [counts]
  (if (>= (counts \1) (counts \0)) \1 \0))

(defn scrubbr [counts]
  (if (< (counts \1) (counts \0)) \1 \0))

(defn rating [lines-in wanted-fn]
  (loop [lines lines-in
         index 0]
    (if-not (seq lines)
      nil
      (let [lines' (transpose lines)
            counts (frequencies (nth lines' index))
            wanted (wanted-fn counts)
            next-lines (filter (fn [l] (= wanted (nth l index))) lines)]
        (if (= 1 (count next-lines))
          (Integer/parseInt (first next-lines) 2)
          (recur next-lines (inc index)))))))

(comment
  (part1)
  (* (rating (read-input-lines 3 identity) oxygen)
     (rating (read-input-lines 3 identity) scrubbr))
  )
