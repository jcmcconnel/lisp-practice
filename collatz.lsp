(defun collatz (n)
  (format t "~2d " n)
 (if (> n 1) 
    (if (evenp n)
      (collatz (/ n 2))
      (collatz (+ (* 3 n) 1))
    )
    1
 )
)
(format t "Enter a number: ~%")
(collatz (read))