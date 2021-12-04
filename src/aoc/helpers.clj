(ns aoc.helpers
  (:require
   [clojure.string :as str]))

(defn read-input-lines
  ([day parse-line]
   (read-input-lines day parse-line false))
  ([day parse-line is-test]
   (->> (str "input_" day (when is-test "_test") ".txt")
        slurp
        str/split-lines
        (map parse-line))))
