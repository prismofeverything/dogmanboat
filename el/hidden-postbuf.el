(defconst sclang-hidden-post-buffer (concat " " sclang-post-buffer))

(defun sclang-recover-postbuffer ()
  (interactive)
  (kill-buffer sclang-post-buffer)
  (make-indirect-buffer sclang-hidden-post-buffer sclang-post-buffer t))

(defun sclang-init-post-buffer ()
  "Initialize post buffer."
  (when (not (get-buffer sclang-hidden-post-buffer))
      (get-buffer-create sclang-hidden-post-buffer))
  (with-current-buffer sclang-hidden-post-buffer
    ;; setup sclang mode
    (sclang-mode)
    (set (make-local-variable 'font-lock-fontify-region-function)
	 (lambda (&rest args)))
    ;; setup compilation mode
    (compilation-minor-mode)
    (set (make-variable-buffer-local 'compilation-error-screen-columns) nil)
    (set (make-variable-buffer-local 'compilation-error-regexp-alist)
	 (cons (list sclang-parse-error-regexp 2 3 4) compilation-error-regexp-alist))
    (set (make-variable-buffer-local 'compilation-parse-errors-function)
	 (lambda (limit-search find-at-least)
	   (compilation-parse-errors limit-search find-at-least)))
    (set (make-variable-buffer-local 'compilation-parse-errors-filename-function)
	 (lambda (file-name)
	   file-name))
    (kill-buffer sclang-post-buffer)
    (make-indirect-buffer sclang-hidden-post-buffer sclang-post-buffer t)
    (sclang-clear-post-buffer)
    (sclang-show-post-buffer)))

(defun sclang-get-postbuffer ()
  ;; redefinition of the scel version, using hidden backup postbuffer
  (let ((buf (get-buffer sclang-post-buffer)))
    (if buf
	buf
      (if (not (get-buffer sclang-hidden-post-buffer))
	  (error "sclang postbuffer has been unexpectedly deleted")
	(progn (make-indirect-buffer
		sclang-hidden-post-buffer sclang-post-buffer t)
	       (get-buffer sclang-post-buffer))))))

(defun sclang-start ()
  "Start SuperCollider process."
  ;; redefinition of the scel version, using hidden backup postbuffer
  (interactive)
  (sclang-stop)
  (sit-for 1)
  (sclang-init-post-buffer)
  (sclang-start-command-process)
  (let ((process-connection-type nil))
    (let ((proc (apply 'start-process
		       sclang-process sclang-hidden-post-buffer
		       sclang-program (sclang-make-options))))
      (set-process-sentinel proc 'sclang-process-sentinel)
      (set-process-filter proc 'sclang-process-filter)
      (set-process-coding-system proc 'mule-utf-8 'mule-utf-8)
      (process-kill-without-query proc)
      proc)))

