(require '[clojure.string])

(defn read_coordinate [e]
  (map (fn [x] (Integer/parseInt x)) (clojure.string/split e #",")))

(defn read_input []
  (map read_coordinate
   (clojure.string/split-lines
   (slurp "/home/gdenhez/git/adventofcode/2022/day18/input_test.txt"))))

(read_input)

