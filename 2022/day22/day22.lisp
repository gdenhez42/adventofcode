(defun read_input ()
    (let ((in (open "input_test.txt" :if-does-not-exist nil)) (input (list)))
        (when in
            (loop for line = (read-line in nil)
            
            while line do 
                (setf input (cons line input)))
            (close in))
        input
    ))

(defvar input (read_input))


(defvar command (car input))
(write command)
(terpri)

(let ((input_map (reverse (cdr (cdr input)))) (width 0) (height 0))
    (loop for line in input_map
      do
      (setf l (length line))
      (if (> l width) (setf width l))
      (setf height (+ 1 height)))
    (write width)
    (terpri)
    (write height)
    (terpri)
)
