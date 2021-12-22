(ns aoc.day8
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.set :as set]
   [clojure.string :as str]))

(defn parse-line [l]
  (let [[input output]
        (str/split l #" \| ")]
    [input output]))

(comment
  (parse-line "dgabe fdace acbfge efdcabg egcda cge gc beadcg febgad gdbc | dgcb gebadc adbge gdace"))

;; part 1
(->> (str/split (->> (read-input-lines 8 parse-line)
                     (map second)
                     (interpose " ")
                     (apply str))
                #" ")
     (map count)
     (filter #{2 3 4 7})
     count)

; numbers with 5 letters:
; Overlap with numbers with 2,3 and 4 letters:
; 3 a-cd-fg  (2, 3, 3)
; 2 a-cde-g  (1, 2, 2)
; 5 ab-d-fg  (1, 2, 3)
; numbers with 6 letters:
; 0: abc-efg  (2, 3, 3)
; 6: ab-defg  (1, 2, 3)
; 9: abcd-fg  (2, 3, 4)
(def l5
  {[2 3 3] 3
   [1 2 2] 2
   [1 2 3] 5})
(def l6
  {[2 3 3] 0
   [1 2 3] 6
   [2 3 4] 9})

(defn overlap [p l->p]
  (let [ps (into #{} p)]
    [(count (set/intersection ps (first (l->p 2))))
     (count (set/intersection ps (first (l->p 3))))
     (count (set/intersection ps (first (l->p 4))))]))

(defn deduct
  [line]
  (let [l->p (group-by count (map #(into #{} %) line))]
    (into {} (for [l line]
               (do
                 (case (count l)
                   2 [(sort l) 1]
                   3 [(sort l) 7]
                   4 [(sort l) 4]
                   5 [(sort l) (l5 (overlap l l->p))]
                   6 [(sort l) (l6 (overlap l l->p))]
                   7 [(sort l) 8]))))))

(defn handle-line [[input output]]
  (let [lookup (deduct (str/split input #" "))]
    (apply str (for [pattern (str/split output #" ")] (str (lookup (sort pattern)))))))

(comment
  (->> (read-input-lines 8 parse-line)
       (map handle-line)
       (map parse-long)
       (reduce +)))
