(require '[clojure.string])
(require '[clojure.set])

(defn read_coordinate [e]
  (into [] (map (fn [x] (Integer/parseInt x)) (clojure.string/split e #","))))

(defn read_input []
  (map read_coordinate
   (clojure.string/split-lines
   (slurp "/home/gdenhez/git/adventofcode/2022/day18/input.txt"))))

(def input (read_input))

(def x_plans (group-by (fn [[x _ _]] x) input))
(def y_plans (group-by (fn [[_ y _]] y) input))
(def z_plans (group-by (fn [[_ _ z]] z) input))

(defn is_not_blocked_above [[x y z]]
  (let [elem (x_plans (- x 1))]
    (if (= elem nil) true
        (= (some (fn [[_ ey ez]] (and (= y ey) (= z ez))) elem) nil))))

(defn is_not_blocked_below [[x y z]]
  (let [elem (x_plans (+ x 1))]
    (if (= elem nil) true
        (= (some (fn [[_ ey ez]] (and (= y ey) (= z ez))) elem) nil))))

(defn is_not_blocked_left [[x y z]]
  (let [elem (y_plans (- y 1))]
    (if (= elem nil) true
        (= (some (fn [[ex _ ez]] (and (= x ex) (= z ez))) elem) nil))))

(defn is_not_blocked_right [[x y z]]
  (let [elem (y_plans (+ y 1))]
    (if (= elem nil) true
        (= (some (fn [[ex _ ez]] (and (= x ex) (= z ez))) elem) nil))))

(defn is_not_blocked_front [[x y z]]
  (let [elem (z_plans (+ z 1))]
    (if (= elem nil) true
        (= (some (fn [[ex ey _]] (and (= x ex) (= y ey))) elem) nil))))

(defn is_not_blocked_back [[x y z]]
  (let [elem (z_plans (- z 1))]
    (if (= elem nil) true
        (= (some (fn [[ex ey _]] (and (= x ex) (= y ey))) elem) nil))))

(def nb_tiles_x
  (reduce (fn [acc [_ plan]]
            (+ (count (filter is_not_blocked_above plan))
               (count (filter is_not_blocked_below plan))
               acc))
          0
          x_plans))

(def nb_tiles_y
  (reduce (fn [acc [_ plan]]
            (+ (count (filter is_not_blocked_right plan))
               (count (filter is_not_blocked_left plan))
               acc))
          0
          y_plans))

(def nb_tiles_z
  (reduce (fn [acc [_ plan]]
            (+ (count (filter is_not_blocked_front plan))
               (count (filter is_not_blocked_back plan))
               acc))
          0
          z_plans))

;; part 1
(+ nb_tiles_x nb_tiles_y nb_tiles_z)
