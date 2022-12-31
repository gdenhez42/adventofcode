(defstruct coord 
   x
   y
)


(defun read_input ()
    (let ((in (open "input_test.txt" :if-does-not-exist nil)) (input (list)))
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

(defun find-first-non-space (board)
    (let ((j 0))
        (loop while (char= (aref board 0 j) #\space)
            do (setf j (+ j 1))
        )
        (write (aref board 0 j))
        (make-coord :x j :y 0)
    )
)


(defvar input (read_input))
(defvar path (car input))
(defvar board (build_board input))
(defvar starting_pos (find-first-non-space board))
(terpri)


(write board)
