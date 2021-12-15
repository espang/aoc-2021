(ns aoc.day15
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.data.priority-map :refer [priority-map]]
   [clojure.string :as str]))

(defn parse-line [l]
  (->> l
       (map str)
       (mapv parse-long)))

(defn value-at [[row col] matrix]
  (nth (nth matrix row []) col nil))

(defn neighbours [[row col] matrix]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map (fn [[dr dc]]
              [(+ row dr)
               (+ col dc)]))
       (map (fn [pos] [pos (value-at pos matrix)]))
       (filter (comp some? second))))

(defn shortest-path 
  ([start matrix target]
   (shortest-path start matrix target neighbours))
  ([[x y] matrix target neighbours]
   (loop [p       (priority-map [x y] 0)
          visited {}]
     (if (empty? p)
       :no-path
       (let [[pos distance] (peek p)
             visisted' (assoc visited pos distance)
             next      (neighbours pos matrix)]
         (if (= pos target)
           distance
           (if (contains? visited pos)
             (recur (dissoc p pos) visited)
             (recur (reduce (fn [p' [pos' d]]
                              (let [distance' (+ distance d)
                                    val       (p' pos' nil)]
                                (if (or (contains? visited pos')
                                        (and (some? val)
                                             (>= distance' val)))
                                  p'
                                  (assoc p' pos' distance'))))
                            p
                            next)
                    visisted'))))))))

(comment
  (def mat (->> (read-input-lines 15 parse-line)
                (into [])))
  (shortest-path [0 0]
                 mat
                 [99 99]))

(defn value-at2 [[row col] matrix]
  "project the row col into the matrix"
  (if (or (neg? row) (neg? col))
    nil
    (let [nrows (count matrix)
          ncols (count (first matrix))
          row'  (mod row nrows)
          col'  (mod col ncols)
          rowv  (quot row nrows)
          colv  (quot col ncols)]
      (if (or (> rowv 4)
              (> colv 4))
        nil
        (when-let [v (nth (nth matrix row' []) col' nil)]
          (if (= 9 (+ v rowv colv))
            9
            (mod (+ v rowv colv) 9)))))))

(defn neighbours2 [[row col] matrix]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map (fn [[dr dc]]
              [(+ row dr)
               (+ col dc)]))
       (map (fn [pos] [pos (value-at2 pos matrix)]))
       (filter (comp some? second))))

(comment
  (def mat (->> (read-input-lines 15 parse-line)
                (into [])))
  (shortest-path [0 0]
                 mat
                 [499 499]
                 neighbours2))

