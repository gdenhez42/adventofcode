(defstruct coord 
    x
    y
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
        (make-coord :x j :y 0)
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

(defun rotate-left (d)
    (case d
        (#\> #\^)
        (#\^ #\<)
        (#\< #\v)
        (#\v #\>)
    )
)

(defun rotate-right (d)
    (case d
        (#\> #\v)
        (#\^ #\>)
        (#\< #\^)
        (#\v #\<)
    )
)

(defun next-tile-right (x y board)
    (let ( (size (array-dimension board 1))  )
        (let ( (j (mod (+ x 1) size)) )
            (loop while (char= (aref board y j) #\space)
                do (setf j (mod (+ j 1) size))
            )
            j
        )
    )
)

(defun next-tile-left (x y board)
    (let ( (size (array-dimension board 1))  )
        (let ( (j (mod (- x 1) size)) )
            (loop while (char= (aref board y j) #\space)
                do (setf j (mod (- j 1) size))
            )
            j
        )
    )
)

(defun next-tile-down (x y board)
    (let ( (size (array-dimension board 0))  )
        (let ( (i (mod (+ y 1) size)) )
            (loop while (char= (aref board i x) #\space)
                do (setf i (mod (+ i 1) size))
            )
            i
        )
    )
)

(defun next-tile-up (x y board)
    (let ( (size (array-dimension board 0))  )
        (let ( (i (mod (- y 1) size)) )
            (loop while (char= (aref board i x) #\space)
                do (setf i (mod (- i 1) size))
            )
            i
        )
    )
)

(defun move-right (pos nb board)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (i 0) (next_x (next-tile-right (coord-x pos) (coord-y pos) board)) )
        (loop while (and (char= (aref board y next_x) #\.) (< i nb))
            do
            (setf i (+ i 1))
            (setf x next_x)
            (setf next_x (next-tile-right x y board))
        )
        (make-coord :x x :y y)
    )
)

(defun move-left (pos nb board)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (i 0) (next_x (next-tile-left (coord-x pos) (coord-y pos) board)) )
        (loop while (and (char= (aref board y next_x) #\.) (< i nb))
            do
            (setf i (+ i 1))
            (setf x next_x)
            (setf next_x (next-tile-left x y board))
        )
        (make-coord :x x :y y)
    )
)

(defun move-down (pos nb board)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (i 0) (next_y (next-tile-down (coord-x pos) (coord-y pos) board)) )
        (loop while (and (char= (aref board next_y x) #\.) (< i nb))
            do
            (setf i (+ i 1))
            (setf y next_y)
            (setf next_y (next-tile-down x y board))
        )
        (make-coord :x x :y y)
    )
)

(defun move-up (pos nb board)
    (let ( (x (coord-x pos)) (y (coord-y pos)) (i 0) (next_y (next-tile-up (coord-x pos) (coord-y pos) board)) )
        (loop while (and (char= (aref board next_y x) #\.) (< i nb))
            do
            (setf i (+ i 1))
            (setf y next_y)
            (setf next_y (next-tile-up x y board))
        )
        (make-coord :x x :y y)
    )
)


(defun move (d pos nb board)
    (case d
        (#\> (move-right pos nb board))
        (#\^ (move-up pos nb board))
        (#\< (move-left pos nb board))
        (#\v (move-down pos nb board))
    )
)


(defvar input (read_input))
(defvar path (car input))
(defvar board (build_board input))
(defvar starting_pos (find-starting-pos board))
(write starting_pos)
(terpri)

(let ( (p path) (d #\>) (pos starting_pos))
    (loop while (string/= "" p)
        do
        (let ( (c (read-path p)) )
            (case (aref (command-current c) 0)
                (#\L (setf d (rotate-left d)))
                (#\R (setf d (rotate-right d)))
                (t (setf pos (move d pos (parse-integer (command-current c)) board)))
            )

            (write (command-current c))
            (terpri)
            (write d)
            (terpri)
            (write pos)
            (terpri)
            (terpri)
            (setf p (command-rest c))
        )
    )
    (let ( (row (+ (coord-y pos) 1))
           (col (+ (coord-x pos) 1))
           (final_facing (case d (#\> 0) (#\^ 3) (#\< 2) (#\v 1)))
         )
        (write (+ (* 1000 row) (* 4 col) final_facing))
    )
)
