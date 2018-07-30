(defstruct time_c hrs mins)

(defun parse-time (str)
  (setq index (do ((i 0 (+ i 1)))
		  ((char= #\: (char str i) ; Split by ':' delimiter
		(return i) 0))))
  (make-time_c
   :hrs (parse-integer (subseq str 0 (- 1 index)))
   :mins (parse-integer (subseq str (+ index 2)))))

(defun calctime (S to_elapse)
  (setq offset (+ (* (time_c-hrs S) 60) (time_c-mins S)))
  (setq total (+ offset to_elapse))
  (make-time_c
   :hrs (floor (/ total 60))
   :mins (mod total 60)))

; Main
(setq start (parse-time (pop *args*)))
(setq elapse (parse-integer (pop *args*)))
(setq end (calctime start elapse))

(format t "start: ~2,'0D:~2,'0D ~@D~%" (time_c-hrs start)
	(time_c-mins start) elapse)
(format t "End: ~2,'0D:~2,'0D~%" (time_c-hrs end) (time_c-mins end))
