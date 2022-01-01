(ns aoc.day10
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

(def open->close 
  {\( \)
   \{ \}
   \[ \]
   \< \>})

(def close->open
  (reduce-kv #(assoc %1 %3 %2)
             {} 
             open->close))

(apply str (map open->close [\[ \{]))

(defn valid? [line]
  (loop [line line
         openings []]
    (if-not (seq line)
      [true (apply str (reverse (map open->close openings)))]
      (if-let [opening (open->close (first line))]
        (recur (rest line)
               (conj openings (first line)))
        (if-let [closing (close->open (first line))]
          (if (= closing (last openings))
            (recur (rest line)
                   (pop openings))
            [false (first line)])
          [false :invalid])))))

(comment 
  (valid? "{([(<{}[<>[]}>{[]{[(<()>")
  (valid? "[[<[([]))<([[{}[[()]]]")
  (valid? "[({(<(())[]>[[{[]{<()<>>"))

(def points {\) 3 \] 57 \} 1197 \> 25137})

(defn part1 [lines]
  (->> lines
       (map valid?)
       (filter (fn [[ok _]] (not ok)))
       (map second)
       (map points)
       (reduce +)))

(comment
  (part1 (read-input-lines 10 identity)))

(comment
  (valid? "[({(<(())[]>[[{[]{<()<>>"))

(def points2 {\) 1 \] 2 \} 3 \> 4})

(defn score 
  ([completion]
   (score completion 0))
  ([completion x]
   (if-not (seq completion)
     x
     (recur (rest completion)
            (+ (* 5 x)
               (points2 (first completion)))))))

(defn part2 [lines]
  (let [scores (->> lines
                    (map valid?)
                    (filter (fn [[ok _]] ok))
                    (map second)
                    (map score)
                    sort)
        n (count scores)]
    (nth scores (quot n 2))))

(comment
  (part2 (read-input-lines 10 identity true))
  (part2 (read-input-lines 10 identity)))

