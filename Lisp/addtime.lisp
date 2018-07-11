(defstruct time_c hrs mins)

(defun calctime (S to_elapse)
  (setq offset (+ (* (time_c-hrs S) 60) (time_c-mins S)))
  (setq total (+ offset to_elapse))
  (make-time_c
   :hrs (floor (/ total 60))
   :mins (mod total 60)))

(setq start (make-time_c :hrs 1 :mins 30))
(setq elapse -60)
(setq end (calctime start elapse))

(format t "start: ~2,'0D:~2,'0D ~D~%" (time_c-hrs start)
	(time_c-mins start) elapse)
(format t "End: ~2,'0D:~2,'0D~%" (time_c-hrs end) (time_c-mins end))
