(ns aoc.day14
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

(def t-start "NNCB")
(def start "OOBFPNOPBHKCCVHOBCSO")

(defn parse-line [line]
  (let [[pair insert] (str/split line #" -> ")]
    [(vec pair) (first insert)]))

(defn do-n [n insertions start]
  (loop [n n
         polymer start]
    (if (zero? n)
      polymer
      (let [to-insert (->> polymer
                           (partition 2 1)
                           (map insertions))]
        (recur (dec n)
               (conj (into [] (interleave polymer to-insert))
                     (last polymer)))))))

(defn result [polymer]
  (let [freqs (-> polymer
                  frequencies
                  vals
                  sort)]
    (- (last freqs) (first freqs))))

(comment
  (time (-> (do-n 10
                  (->> (read-input-lines 14 parse-line)
                       (into {}))
                  start)
            result)))

(defn do2-n [n insertions start]
  (let [;; An insertion like AA->B creates 2 pairs from 1: AB, BA when this is done the B is counted twice
        tuple->tuples (reduce-kv (fn [acc [c1 c2] v]
                                   (assoc acc (str c1 c2) [(str c1 v) (str v c2)]))
                                 {}
                                 insertions)
        ;; Count pairs and count the single characters that are counted double
        counter (reduce (fn [acc k]
                          (-> acc
                              (update (apply str k) (fnil inc 0))
                              (update (first k) (fnil inc 0))))
                        ;; The first character is not counted double, each following
                        ;; pair counts the first character twice
                        {(first start) -1}
                        (partition 2 1 start))]
    (loop [n n
           c counter]
      (if (zero? n)
        (reduce-kv (fn [acc k v]
                     ;; This counter contains pairs (strings) that will count to the occurences of characters
                     ;; and single characters that counted the double counting and will be substracted.
                     (if (string? k)
                       (let [[c1 c2] k]
                         (-> acc
                             (update c1 (fnil + 0) v)
                             (update c2 (fnil + 0) v)))
                       (-> acc
                           (update k (fnil - 0) v))))
                   {}
                   c)
        (recur (dec n)
               (reduce-kv (fn [acc k v]
                            ;; A string represents a pair that will be changed into two pairs, 
                            ;; counting the inserted char twice.
                            ;; A character represents previously double counted characters, this
                            ;; count is simply kept.
                            (if (string? k)
                              (let [[t1 t2] (tuple->tuples k)]
                                (-> acc
                                    (update t1 (fnil + 0) v)
                                    (update t2 (fnil + 0) v)
                                    (update (first t2) (fnil + 0) v)))
                              (update acc k (fnil + 0) v)))
                          {}
                          c))))))

(comment
  (time (-> (do-n 10
                  (->> (read-input-lines 14 parse-line true)
                       (into {}))
                  t-start)
            frequencies))
  (time (-> (do2-n 40
                   (->> (read-input-lines 14 parse-line)
                        (into {}))
                   start)
            vals
            sort))
  (- 3403627694467 1043328798690))