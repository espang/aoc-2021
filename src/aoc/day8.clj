(ns aoc.day8
  (:require
   [aoc.helpers :refer [read-input-lines]]
   [clojure.string :as str]))

;; dgabe fdace acbfge efdcabg egcda cge gc beadcg febgad gdbc | dgcb gebadc adbge gdace
(defn parse-line [l]
  (let [[input output]
        (str/split l #" \| ")]
    [input output]))

(comment
  (parse-line "dgabe fdace acbfge efdcabg egcda cge gc beadcg febgad gdbc | dgcb gebadc adbge gdace"))



(def notes ["acedgfb" "cdfbe" "gcdfa" "fbcad"
            "dab" "cefabd" "cdfgeb" "eafb" "cagedb" "ab"
            "cdfeb" "fcadb" "cdfeb" "cdbaf"])

(def numbers
  {0 (into #{} "abcefg")
   1 (into #{} "cf")
   2 (into #{} "acdeg")
   3 (into #{} "acdfg")
   4 (into #{} "bcdf")
   5 (into #{} "abdfg")
   6 (into #{} "abdefg")
   7 (into #{} "acf")
   8 (into #{} "abcdefg")
   9 (into #{} "abcdfg")})


