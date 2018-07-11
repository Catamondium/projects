(defun fibonacci (num) ; fibonacci iterative solution
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next)))
      ((= num n) cur)))

;; main()
(princ "Enter lower bound: ")
(setq i (read))

(princ "Enter upper bound: ")
(setq in (read))

(loop
   (format t "Fib: ~T~D~T==>~T~D~%" i (fibonacci i))
   (setq i (+ i 1))
   (if (> i in) (return)))
