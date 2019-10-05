;;;; CL addtime implementation
(load "~/quicklisp/setup.lisp") ; how to do properly?
(ql:quickload "split-sequence")

(defclass time_c ()
  ( (hrs :accessor time-hrs
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
    ((vals (split-sequence:split-sequence #\: str)))
    (make-instance 'time_c
      :hrs (parse-integer (pop vals))
      :mins (parse-integer (pop vals)))))

(defun tInt (S)
 (+ (* (time-hrs S) 60) (time-mins S)))

(defun calctime (S to_elapse)
 (let*
    ((total (+ (tInt S) to_elapse)))
    (make-instance 'time_c
      :hrs (floor (/ total 60))
      :mins (mod total 60))))

;;; Main
(setq start (parse-time (pop *args*)))
(setq estr (pop *args*))
(setq elapse
  (if (find #\: estr)
    (tInt (parse-time estr))
    (parse-integer estr)))

(format t "Start: ~S ~@D~%End: ~S~%" start elapse (calctime start elapse))
