(defun fibonacci (num) ; fibonacci iterative solution
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= num n) cur)))

;; main()
(princ "Enter lower bound: ")
(setq low (read))

(princ "Enter upper bound: ")
(setq high (read))

(do ((i low (+ 1 i))) ; Iterating part
    ((> i high) i 0)  ; Check part
  (format t "Fib: ~T~D~T==>~T~D~%" i (fibonacci i))) ; Body
