(defstruct coord 
    x
    y
)

(defstruct command
    current
    rest
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




(defvar input (read_input))
(defvar path (car input))
(defvar board (build_board input))
(defvar starting_pos (find-starting-pos board))
(terpri)


(let ( (p path) (d ">") (pos starting_pos))
    (loop while (string/= "" p)
        do
        (let ( (c (read-path p)) )
            (write (command-current c))
            (terpri)
            (setf p (command-rest c))
        )
    )
)