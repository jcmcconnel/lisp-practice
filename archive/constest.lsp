(defun constest (n)
  "Recursively builds a list"
  (if (zerop n)
    nil
    (cons n (constest (- n 1)))
    )
  )
