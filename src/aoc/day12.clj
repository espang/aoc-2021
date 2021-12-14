(ns aoc.day12
  (:require
     [aoc.helpers :refer [read-input-lines]]
     [clojure.string :as str]))

(def start "start")
(def end "end")
(defn start? [node]
  (= node start))
(defn end? [node]
  (= node end))
(defn small? [node]
  (= node (str/lower-case node)))
(defn big? [node] (not (or (small? node)
                           (start? node)
                           (end? node))))

(defn parse-line [l]
  (let [[n1 n2] (str/split l #"-")]
    [n1 n2]))

(defn to-graph [edges]
  (reduce (fn [graph [n1 n2]]
            (-> graph
                (update n1 conj n2)
                (update n2 conj n1)))
          {}
          edges))

(defn all-paths [graph]
  (loop [;; elements to look at next
         q (conj (clojure.lang.PersistentQueue/EMPTY) ["start" []])
         paths #{}]
    (if (empty? q)
      paths
      (let [[node path] (peek q)
            smalls (into #{} (filter small? path))
            next-nodes (remove smalls (graph node))]
        (if (end? node)
          (recur (pop q)
                 (conj paths (conj path node)))
          (recur (apply conj
                        (pop q)
                        (map (fn [n] [n (conj path node)]) next-nodes))
                 paths))))))

(comment
  (->> (read-input-lines 12 parse-line true)
       to-graph
       all-paths
       count)
  (->> (read-input-lines 12 parse-line)
       to-graph
       all-paths
       count))

(defn all-paths2 [graph]
  (let [graph (update-vals graph #(remove start? %))]
    (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) ["start"])
           paths #{}]
    (if (empty? q)
      paths
      (let [path (peek q)
            node (last path)
            smalls (frequencies (filter small? path))
            next-nodes (if (some #(< 1 (val %)) smalls)
                         (remove #(contains? smalls %) (graph node))
                         (graph node))]
        (if (end? node)
          (recur (pop q)
                 (conj paths path))
          (recur (apply conj
                        (pop q)
                        (map (fn [n] (conj path n)) next-nodes))
                 paths)))))))

(comment
  (->> (read-input-lines 12 parse-line true)
       to-graph
       all-paths2
       count)
  (->> (read-input-lines 12 parse-line)
       to-graph
       all-paths2
       count))
