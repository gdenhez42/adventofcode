(require '[clojure.string])
(require '[clojure.set])

(defn read_coordinate [e]
  (into [] (map (fn [x] (Integer/parseInt x)) (clojure.string/split e #","))))

(defn read_input []
  (map read_coordinate
       (clojure.string/split-lines
        (slurp "/home/gdenhez/git/adventofcode/2022/day18/input.txt"))))

(def input (set (read_input)))

(def min_x (apply min (map (fn [[x _ _]] x) input)))
(def max_x (apply max (map (fn [[x _ _]] x) input)))
(def min_y (apply min (map (fn [[_ y _]] y) input)))
(def max_y (apply max (map (fn [[_ y _]] y) input)))
(def min_z (apply min (map (fn [[_ _ z]] z) input)))
(def max_z (apply max (map (fn [[_ _ z]] z) input)))

(defn faces [[x y z]]
  (filter (fn [e] (not (contains? input e)))
          (list [(+ x 1) y z] [(- x 1) y z]
                [x (+ y 1) z] [x (- y 1) z]
                [x y (+ z 1)] [x y (- z 1)])))

(defn connect_outside [[x y z] visited]
  (if (or (<= x min_x) (>= x max_x) (<= y min_y) (>= y max_y) (<= z min_z) (>= z max_z))
    [true visited]
    (loop [n (filter (fn [e] (not (contains? visited e))) (faces [x y z]))
           new_visited (clojure.set/union visited (set n))]
      (if (empty? n) [false new_visited]
          (let [[rez nv] (connect_outside (first n) new_visited)]
            (if rez [true nv] (recur (rest n) nv)))))))

(defn count_surface_e [[x y z] visited] 
  (reduce (fn [[cnt visited_acc] e] 
            (if (contains? visited_acc e) 
              (if (visited_acc e) [(+ 1 cnt) visited_acc] [cnt visited_acc])
                (let [[c v] (connect_outside e #{}) 
                      nv (reduce (fn [acc e1] (assoc acc e1 c)) visited_acc v)]
                  (if c [(+ 1 cnt) nv] [cnt nv]))))
            [0 visited] (faces [x y z])))

(first (reduce (fn [[cnt visited_acc] e]
          (let [[nc nv] (count_surface_e e visited_acc)]
            [(+ nc cnt) nv])) [0 (hash-map)] input)
)