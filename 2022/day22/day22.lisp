(defvar input (list))
(let ((in (open "input_test.txt" :if-does-not-exist nil)))
   (when in
      (loop for line = (read-line in nil)
      
      while line do 
        (setf input (cons line input)))
      (close in)
   )
)
(terpri)
(write input)

(defvar command (car input))
(terpri)
(write command)

