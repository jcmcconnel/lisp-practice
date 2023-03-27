; Fun with Lisp!
; I have been trying to learn Lisp.

; Lisp is VERY different than many of the languages in common use today.  

; The easiest way to run this program is in a Linux environment with 'sbcl': Steel-Bank Common Lisp installed.
; You can use the command: 

; $ sbcl --script example.lsp

; Or if you like using the REPL (Read-Evaluate-Print-Loop; Similar to the Python interpreter...):
; Start the REPL by running: 

; $ sbcl

; Then type: 

; $ (load "example.lsp")

; The program will run like normal.
; When you want to exit, type: 

; $ (exit)


; You may notice a lot of recursion here.  Lisp is REALLY good at recursion, because everything is represented as a linked list.
; That's why the language is called Lisp: List Processor

(defun binomial (n r)                                    ; Unlike what you may be used to, the parenthesis starts every "form".  It's like Reverse Polish Notation, if you know what that is.
  "Computes the binomial coefficient of x^r for (1+x)^n" ; The first line of any function can be a description
  (if                                                    ; Open If-statement, it accepts 3 forms: the comparision, true branch, and false branch.
    (or                                                  ; OR
      (zerop r)                                          ; If the "predicate" (parameter) is zero, then return true
      (= r n)                                            ; If r equals n return true
    )                
    1                                                    ; True branch: return 1
    (+                                                   ; False branch: Add 
      (binomial (- n 1) (- r 1))                         ; Call self on n-1 and r-1
      (binomial (- n 1) r)                               ; Call self on n-1 and r
    )
  )                                                      
)                                                        ; Return value is implicitly the result of evaluating the "form".

(defun pascal (n)                                        ; For instance, the return of this function is the list of items "collected" by the loop
  "Compute the nth row of Pascal's Triangle"
  (loop for r from 0 to n                                ; loop is actually a compiler/interpreter macro, which is why it has non-conforming syntax.
      collect (binomial n r)                             ; The compiler expands the macro to a valid complex form before compiling it.
  )                                                      
)

(defun constest (n)                                      ; Every Lisp data item lives in a "cons cell" called an S-expression
  "Recursively builds a list using the cons function"    ; Each cell has a "car" and a "cdr" (don't ask, no one really knows for sure...)
  (if (zerop n)                                          ; The car is the piece of data, and the cdr is the next item, if there is one.
    nil                                                  ; You can use the 'cons' function to build a cell, and if you do it repeatedly, you make a list.
    (cons n (constest (- n 1)))                          
  )
)

(defun constest2 (n)
  "Shows the difference between car and cdr"             ; This is why Lisp is great for writing compilers and text processors.  
  (list 
    (format nil "Full list: ~d~%" (constest n))          ; Traversing a list is as easy as calling 'car' and then passing the 'cdr' in a recursive function call.
    (format nil "Just the car: ~d~%" (car (constest n))) ; The letter 't' means true, and 'nil' means null or false.  
    (format nil "Just the cdr: ~d~%" (cdr (constest n))) ; The format function outputs to 'stdout' (the console) on 't' and just returns the string on 'nil'
  )
)                                                        ; The tildes '~' are directives like with C's printf '%' directives

(defun collatz (n)                                       
  "Computes the Collatz sequence for n"
  (if (> n 1)                                            ; Hopefully you are starting to get the idea.
    (if (evenp n)                                        ; If the predicate is even...
      (cons n (collatz (/ n 2)))                         ; Return a list starting with n, and ending with what comes back from this function...
      (cons n (collatz (+ (* 3 n) 1)))                   ; 
    )
    (cons n nil)                                         ; If n is 1...all well formed lists are supposed to end with nil.
  )
)

(defun fibonacci (n)
  "Outputs the fibonacci sequence of length n"
  (let (a b c fib-seq)                                   ; let provides a scope for variables.  In this case: a, b, c and fib-seq
    (setq a 0)                                           ; setq assigns a value to a variable (again, I don't know where the name came from...)
    (setq b 0)
    (setq c 1)
    (loop 
      (setq fib-seq (append fib-seq  (cons c nil)))      ; append requires both arguments to be a list, so we make a 1 value list to give it.
      (setq a b)                                         
      (setq b c)                                        
      (setq c (+ a b))                                 
      (setq n (- n 1))                                
      (when (<= n 0) (return fib-seq))               
    )                                               
  )                                                
)
; Note how an alternative might have been made using recursion:
; (defun fib (a b n)
;    (if (<= n 0) 
;      (cons (+ a b) nil)
;      (cons (+ a b) (fib b (+ a b) (- n 1)))
;    )
; )
; (defun fibonacci (n)
;   (cons 0 (cons 1 (fib 0 1 n))))

(defun nearestpwr2 (n)
  "Outputs the nearest powers of 2 before and after n"
  (let ((p 0))                                           ; You can assign initial values to variables declared in a let: (let (x 10) (y 3) (z 4) ...
    (loop while (< (expt 2 p) n)                         ; Remember the syntax of loop is not standard
      do (setq p (+ p 1)))
  (list (expt 2 (- p 1)) (expt 2 p))                     ; Return a list of the two powers of 2; Exponentiation: (expt base power) 
 )
)

(defun twosComplement (n)
  "Outputs the two's complement (binary inverse) of n"
  (list                                                  ; Make a list of the string representation, formatted in binary, of the number
    (format nil "~b " n)                                 ; 
    (format nil "~b " (+ 1 (lognor n 0)))                ; Use 'lognor' (Logical NOR) to flip all the bits, then add 1.
  )
)

(defun triangular (n)
  "Compute the nth triangular number n+(n-1)+(n-2), etc" ; Yet another recursive function
  (if (= n 1) 
    1
    (+ n (triangular (- n 1)))
  )
)

(loop while t                                                  ; Now lets have a little user interaction...
   do (let (selection f n (exit-loop nil))
      (format t "~{~a~%~}"                                     ; Output a list of options for the user to try
        (list                                                  ; The ~{ and ~} tell it to repeat for each item in the list
           "Type an option:"                                   ; ~a is for aesthetic
           "   pascal"                                         ; ~% inserts a newline
           "   lisp-list"
           "   car-cdr-demo"
           "   collatz"
           "   fibonacci"
           "   nearest-pow2"
           "   twoscomplement"
           "   triangular"
           "   exit"
        )
      )

      (setq selection (read))
      (cond                                                             ; Lisp even has the ability to use dynamic functions
        ((string= selection 'pascal) (setq f 'pascal))
        ((string= selection 'lisp-list) (setq f 'constest ))
        ((string= selection 'car-cdr-demo) (setq f 'constest2 ))
        ((string= selection 'collatz) (setq f 'collatz ))
        ((string= selection 'fibonacci) (setq f 'fibonacci ))
        ((string= selection 'nearest-pow2) (setq f 'nearestpwr2 ))
        ((string= selection 'twoscomplement) (setq f 'twosComplement ))
        ((string= selection 'triangular) (setq f 'triangular ))
        ((and t) (setq exit-loop t ))
      )
      (if exit-loop 
        (exit) 
        (progn                                                         ; progn allows you to list multiple statements where ordinarily you would only be allowed 1.
           (format t "Enter a number: ~%")
           (setq n (read))
           (format t "Calling function: ~s with ~d~%" f n)
           (format t "~a~%" (funcall f n))                             ; Here we call whichever function the user selected.
        )
      )
   )
)
