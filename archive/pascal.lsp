(defun binomial (n r) 
  "Computes the binomial coefficient of x^r for (1+x)^n"
  (if (or (zerop r) (= r n))
    1
    (+ (binomial (- n 1) (- r 1)) (binomial (- n 1) r))
  )
)

(defun pascal (n)
  "Compute the nth row of Pascal's Triangle"
  (loop for r from 0 to n
      collect (binomial n r) 
  )
)

(format t "~d" (pascal 3))

  
