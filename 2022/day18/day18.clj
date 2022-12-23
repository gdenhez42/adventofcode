(require '[clojure.string])

(defn read_coordinate [e]
  (map (fn [x] (Integer/parseInt x)) (clojure.string/split e #",")))

(defn read_input []
  (map read_coordinate
   (clojure.string/split-lines
   (slurp "/home/gdenhez/git/adventofcode/2022/day18/input_test.txt"))))

(defn edges [x y z]
  (let [edge1 ["x" x]
        edge2 ["x" (+ x 1)]
        edge3 ["y" y]
        edge4 ["y" (+ y 1)]
        edge5 ["z" z]
        edge6 ["z" (+ z 1)]]
    (set (list edge1 edge2 edge3 edge4 edge5 edge6))))


(defn count_edges [elem arr]
  (reduce conj arr #{}))

(apply edges '(1 2 2))

