(ns aoc.helpers)

(defn read-input
  ([day parse-line]
   (read-input day parse-line false))
  ([day parse-line is-test]
   (->> (str "input_" day (when is-test "_test") ".txt")
        slurp
        (map parse-line))))
