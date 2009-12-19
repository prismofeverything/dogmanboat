(defmacro make-proc-reaper (name string)
  `(defun ,name (&optional num)
     (interactive)
     (let ((proc-string (format "%s%s" ,string
				(if (and num (> num 0))
				    (format "<%d>" num) ""))))
       (when (get-process proc-string)
	 (delete-process proc-string)
	 t))))

(make-proc-reaper doscc "OSCclient")
(make-proc-reaper doscs "OSCserver")

(defun d-all ()
  (interactive)
  (loop for i from 0 while (doscc i))
  (loop for i from 0 while (doscs i)))
