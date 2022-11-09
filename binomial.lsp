(defun binomial (n r) 
  "Computes the binomial coefficient of x^r for (1+x)^n"
  (if (or (zerop r) (= r n))
    1
    (+ (binomial (- n 1) (- r 1)) (binomial (- n 1) r))
  )
)
