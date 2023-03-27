(defun collatz (n)
 (if (> n 1) 
    (if (evenp n)
      (collatz (/ n 2))
      (progn 
        (collatz (+ (* 3 n) 1))
        (nearestpwr2 n)
      )
    )
    1
 )
)
(defun nearestpwr2 (n)
  (let ((p 0))
    (loop while (< (expt 2 p) n) 
      do (setq p (+ p 1)))
  (format t "~1d-~1d+~1d " (- n (expt 2 (- p 1))) n (- (expt 2 p) n))
 )
)
(format t "Enter a number: ~%")
(collatz (read))
