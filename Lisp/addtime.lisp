(defclass time_c () 
  ((hrs :accessor time-hrs
	:initarg :hrs
	:type number)
  (mins :accessor time-mins
	:initarg :mins
	:type number)))

(defmethod print-object ((obj time_c) out) ; __repr__ equivalent
    (format out "~2,'0D:~2,'0D" (time-hrs obj) (time-mins obj)))

(defun parse-time (str)
  (let ((index (position #\: str)))
    (make-instance 'time_c
      :hrs (parse-integer (subseq str 0 index))
      :mins (parse-integer (subseq str (+ index 1))))))

(defun calctime (S to_elapse)
  (let* ((offset (+ (* (time-hrs S) 60) (time-mins S)))
  (total (+ offset to_elapse)))
    (make-instance 'time_c
      :hrs (floor (/ total 60))
      :mins (mod total 60))))

; Main
(setq start (parse-time (pop *args*)))
(setq elapse (parse-integer (pop *args*)))
(setq end (calctime start elapse))

(format t "Start: ~S ~@D~%End: ~S~%" start elapse end)
;(format t "End: ~S~%" end)
