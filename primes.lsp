(defun primes (n)
  (let (plist i)
    (setq i 1)
    (loop 
      (when (<= n 0) (return))
      (if (= (mod 
