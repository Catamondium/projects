(defun fibonacci (num) ; fibonacci iterative solution
  (do
    ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= num n) cur)))

; main()
(setq low (parse-integer (pop *args*)))
(setq high (parse-integer (pop *args*)))

(do
  ((i low (+ 1 i)))                ; Iterating part
  ((> i high) i 0)                 ; Check part
  (format t "~D~%" (fibonacci i))) ; Body
