(ns aoc.day4
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

(def random-test [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1])

(def random-real [90 4 2 96 46 1 62 97 3 52 7 35 50 28 31 37 74 26 59 53 82 47 83 80 19 40
                  68 95 34 55 54 73 12 78 30 63 57 93 72 77 56 91 23 67 64 79 85 84 76 10
                  58 0 29 13 94 20 32 25 11 38 89 21 98 92 42 27 14 99 24 75 86 51 22 48 9
                  33 49 18 70 8 87 61 39 16 66 71 5 69 15 43 88 45 6 81 60 36 44 17 41 65])

(defn parse-line [l]
  (->> (-> l
           str/trim
           (str/split #" "))
       (map parse-long)
       (remove nil?)))

(defn board [colls]
  {:found #{}
   :field (mapv vec colls)})

(defn unmarked-fields [{:keys [found field]}]
  (for [row (range 5)
        col (range 5)
        :when (not (contains? found [row col]))]
    ((field row) col)))

(defn update-board [nbr {:keys [field] :as board}]
  (let [found (into #{}
                    (for [row (range 5)
                          col (range 5)
                          :when (= nbr ((field row) col))]
                      [row col]))]
    (update board :found clojure.set/union found)))

(def to-check
  "Contains all rows, columns and diagonals that have to be checked"
  (concat
   ;; rows
   (for [row (range 5)] (for [col (range 5)] [row col]))
   ;; columns
   (for [col (range 5)] (for [row (range 5)] [row col]))))
   ;;diagonals
   ;; (for [row (range 5)] [row row])
   ;; (for [row (range 5)] [row (- 4 row)])))

(defn won? [{:keys [found]}]
  (some? (first (filter #(every? found %) to-check))))

(defn play1 [vals boards]
  (loop [vals vals
         boards boards]
    (if-not (seq vals)
      nil
      (let [boards' (map (partial update-board (first vals)) boards)
            won     (filter won? boards')]
        (if (seq won)
          [(first vals) (first won)]
          (recur (rest vals) boards'))))))

(defn play2 [vals boards]
  (loop [vals vals
         boards boards]
    (if-not (seq vals)
      nil
      (let [boards' (map (partial update-board (first vals)) boards)
            won     (filter won? boards')]
        (if (and (= 1 (count boards') (seq won)))
          [(first vals) (first boards')]
          (recur (rest vals) (remove won? boards')))))))

(defn solve
  ([test]
   (solve 1 test))
  ([p test]
   (let [vals    (if test random-test random-real)
         boards  (->> (read-input-lines 4 parse-line test)
                      (partition 5)
                      (map board))
         f       (if (= p 1) play1 play2)
         [nbr b] (f vals boards)]
     (println nbr b)
     (* (reduce + (unmarked-fields b))
        nbr))))

(comment
  (won? {:found #{[2 4]}, :field [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]]})
  (-> (unmarked-fields {:found #{[2 4]}, :field [[22 13 17 11 0] [8 2 23 4 24] [21 9 14 16 7] [6 10 3 18 5] [1 12 20 15 19]]})
      count)

  (solve 1 true)
  (solve 1 false)
  (solve 2 true))