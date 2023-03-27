(defun triangular (n)
  "Compute the nth triangular number"
  (if (= n 1) 
    1
    (+ n (triangular (- n 1)))
  )
)
