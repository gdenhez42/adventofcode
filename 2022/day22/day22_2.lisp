(defstruct coord 
    x
    y
    d
)

(defstruct command
    current
    rest
)

(defun read_input ()
    (let ((in (open "input.txt" :if-does-not-exist nil)) (input (list)))
        (when in
            (loop for line = (read-line in nil)
            
            while line do 
                (setf input (cons line input)))
            (close in))
        input
    ))

(defun build_board (input)
    (let ((input_map (reverse (cdr (cdr input)))) (width 0) (height 0))
        (loop for line in input_map
            do
            (setf l (length line))
            (if (> l width) (setf width l))
            (setf height (+ 1 height)))

        (let ((i 0)  (v (make-array (list height width) :initial-element #\SPACE))  )
            (loop for line in input_map
                do
                (loop for j from 0 to (- (length line) 1)
                    do
                        (setf (aref v i j) (aref line j))
                )
                (setf i (+ i 1))
            )
            v
        )
        
    )
)

(defun find-starting-pos (board)
    (let ((j 0))
        (loop while (char= (aref board 0 j) #\space)
            do (setf j (+ j 1))
        )
        (make-coord :x j :y 0 :d #\>)
    )
)

(defun rotate-left (pos)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (d (coord-d pos)) )
        (case d
            (#\> (make-coord :x x :y y :d #\^))
            (#\^ (make-coord :x x :y y :d #\<))
            (#\< (make-coord :x x :y y :d #\v))
            (#\v (make-coord :x x :y y :d #\>))
        )
    )
)

(defun rotate-right (pos)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (d (coord-d pos)) )
        (case d
            (#\> (make-coord :x x :y y :d #\v))
            (#\^ (make-coord :x x :y y :d #\>))
            (#\< (make-coord :x x :y y :d #\^))
            (#\v (make-coord :x x :y y :d #\<))
        )
    )
)

; hardcoded for my input cuz 3D visualisation is hard!!!
;  16
;  4
; 35
; 2
(defun next-tile (pos)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (d (coord-d pos)) )
        (cond
            ; face 1 to 2
            ((and (char= d #\^) (= y 0) (>= x 50) (< x 100))
                (make-coord :x 0 :y (+ 150 (- x 50)) :d #\>))

            ; face 1 to 3
            ((and (char= d #\<) (< y 50) (= x 50))
                (make-coord :x 0 :y (- 149 y) :d #\>))

            ; face 2 to 1
            ((and (char= d #\<) (>= y 150) (= x 0))
                (make-coord :x (+ 50 (- y 150)) :y 0 :d #\v))

            ; face 2 to 5
            ((and (char= d #\>) (>= y 150) (= x 49))
                (make-coord :x (+ 50 (- y 150)) :y 149 :d #\^))

            ; face 2 to 6
            ((and (char= d #\v) (= y 199) (< x 50))
                (make-coord :x (+ x 100) :y 0 :d #\v))

            ; face 3 to 1
            ((and (char= d #\<) (>= y 100) (< y 150) (= x 0))
                (make-coord :x 50 :y (- 149 y) :d #\>))

            ; face 3 to 4
            ((and (char= d #\^) (= y 100) (< x 50))
                (make-coord :x 50 :y (+ 50 x) :d #\>))

            ; face 4 to 3
            ((and (char= d #\<) (>= y 50) (< y 100) (= x 50))
                (make-coord :x (- y 50) :y 100 :d #\v))

            ; face 4 to 6
            ((and (char= d #\>) (>= y 50) (< y 100) (= x 99))
                (make-coord :x (+ 100 (- y 50)) :y 49 :d #\^))

            ; face 5 to 2
            ((and (char= d #\v) (= y 149) (>= x 50) (< x 100))
                (make-coord :x 49 :y (+ 150 (- x 50)) :d #\<))

            ; face 5 to 6
            ((and (char= d #\>) (>= y 100) (< y 150) (= x 99))
                (make-coord :x 149 :y (- 149 y) :d #\<))

            ; face 6 to 2
            ((and (char= d #\^) (= y 0) (>= x 100))
                (make-coord :x (- x 100) :y 199 :d #\^))

            ; face 6 to 4
            ((and (char= d #\v) (= y 49) (>= x 100))
                (make-coord :x 99 :y (+ 50 (- x 100)) :d #\<))

            ; face 6 to 5
            ((and (char= d #\>) (< y 50) (= x 149))
                (make-coord :x 99 :y (- 149 y) :d #\<))

            ; move right
            ((char= d #\>) (make-coord :x (+ x 1) :y y :d #\>))

            ; move left
            ((char= d #\<) (make-coord :x (- x 1) :y y :d #\<))

            ; move up
            ((char= d #\^) (make-coord :x x :y (- y 1) :d #\^))

            ; move down
            ((char= d #\v) (make-coord :x x :y (+ y 1) :d #\v))
        )
    )
)


(defun move (pos nb board)
    (let ( (i 0) (next_pos (next-tile pos)) (p pos) )
        (loop while (and (char= (aref board (coord-y next_pos) (coord-x next_pos)) #\.) (< i nb))
            do
            (setf i (+ i 1))
            (setf p next_pos)
            (setf next_pos (next-tile p))
        )
        p
    )
)

(defun read-path (path)
    (let ( (l (length path)) (i 0) )
        (loop while (and (< i l) (char>= (aref path i) #\0) (char<= (aref path i) #\9))
            do (setf i (+ i 1))
        )
        (case i
            (0 (make-command :current (subseq path 0 1) :rest (subseq path 1)))
            (t (make-command :current (subseq path 0 i) :rest (subseq path i)))
        )
    )
)


(defvar input (read_input))
(defvar path (car input))
(defvar board (build_board input))
(defvar starting_pos (find-starting-pos board))


(let ( (p path) (pos starting_pos) )
    (loop while (string/= "" p)
        do
        (let ( (c (read-path p)) )
            (case (aref (command-current c) 0)
                (#\L (setf pos (rotate-left pos)))
                (#\R (setf pos (rotate-right pos)))
                (t (setf pos (move pos (parse-integer (command-current c)) board)))
            )
            (setf p (command-rest c))
        )
    )
    (let ( (row (+ (coord-y pos) 1))
           (col (+ (coord-x pos) 1))
           (final_facing (case (coord-d pos) (#\> 0) (#\^ 3) (#\< 2) (#\v 1)))
         )
        (write (+ (* 1000 row) (* 4 col) final_facing))
    )
)

; too low 161037