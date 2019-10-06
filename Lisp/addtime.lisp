;;;; CL addtime implementation

(defun split-sequence (target subject)
  (let ((pos (position target subject)))
    (if (null pos)
      (list subject)
      (nconc
        (list (subseq subject 0 pos))
        (split-sequence target (subseq subject (+ pos 1)))))))

(defun divmod (x y)
  (list (floor (/ x y)) (mod x y)))

(defclass time_c ()
  ((hrs :accessor time-hrs
        :initarg :hrs
        :initform 0
        :type number)

   (mins :accessor time-mins
         :initarg :mins
         :initform 0
         :type number)))

(defmethod print-object ((obj time_c) out) ; __repr__ equivalent
    (format out "~2,'0D:~2,'0D" (time-hrs obj) (time-mins obj)))

(defun parse-time (str)
  (let*
    ((vals (split-sequence #\: str)))
    (make-instance 'time_c
      :hrs (parse-integer (pop vals))
      :mins (parse-integer (pop vals)))))

(defun tInt (S)
 (+ (* (time-hrs S) 60) (time-mins S)))

(defun calctime (S to_elapse)
 (let*
    ((total (+ (tInt S) to_elapse))
     (parts (divmod total 60)))
    (make-instance 'time_c
      :hrs (pop parts)
      :mins (pop parts))))

;;; Main
(setq start (parse-time (pop *args*)))
(setq estr (pop *args*))
(setq elapse
  (if (find #\: estr)
    (tInt (parse-time estr))
    (parse-integer estr)))

(format t "Start: ~S ~@D~%End:   ~S~%" start elapse (calctime start elapse))
