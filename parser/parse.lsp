(defun get-body (start-key end-key str)
  (let ((i 0) (c) (body)) 
    (loop while (< i (length str))
       do (progn 
            (cond 
            ((string= c end-key) (return-from get-body body))
            ( t 
               (setq body (concatenate 'string body c))
               (if (string= c start-key) 
                 (let (child) 
                   (setq child (get-body start-key end-key (subseq str i)))
                   (setq body (concatenate 'string body child end-key))
                   (setq i (+ 1 (+ (length child) i)))
                 )
               )
            )
         )
         (setq i (+ i 1))
         (setq c (subseq str i (+ i 1)))
       )
    )
  )
)

(defun get-next-block (start-key end-key text)
  (let ((i 0) (c) (list-of-items))
    (loop while (< i (length text))
    do (progn 
          (if (string= start-key (subseq text i (+ i 1)))
              (return-from get-next-block (get-body start-key end-key (subseq text i)))
          )
          (setq i (+ i 1))
      ) 
    )
  )
)

(defun parse-for (token text)
   (let ((i 0) (output) (temp))
     (setq output (cons token nil))
     ;(setq output (format nil "{~%   token: ~a,~%" token))
     (loop while (< i (length text))
        do (if (string= token (subseq text i (+ i (length token))))
          (progn 
             (setq temp (get-next-block "(" ")" (subseq text (+ i (length token)))))
             (setq i (+ i (length temp)))
             (push temp output)
             ;(setq output (concatenate 'string output (format nil "   condition: ~a, ~%" temp)))
             ;(setq output (concatenate 'string output (format nil "   body: ~a~%}" 
             (push (get-next-block "{" "}" (subseq text (+ i (length token)))) output)
             (return-from parse-for output)
          ) 
          (setq i (+ i 1))
        )
     )
   )
)

(defun appian-road (token text)
   (let ((i 0) (whole-list) (body) (n) (tag) (result))
     (setq whole-list (parse-for token text)) 
     (setq body (pop whole-list))
     (setq n (parse-integer (pop whole-list)))
     (setq tag (pop whole-list))
     (cond ((string= tag "repeat") 
         (loop while (< i n)
        do (progn 
             (setq result (concatenate 'string result body))
             (setq i (+ i 1))
           )
         )
         (format t "~a" result)
         )
     )
   )
)
(appian-road "repeat" "repeat(5){a!gridColumn(
  label: fv!row[fv!item],
  contents: {
     a!textField(
        label: \"test\",
        value: \"text\",
    ),
  },
),
}")
