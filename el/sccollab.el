(defcustom sccollab-timestap-lambda current-time
  "function to generate the timestamps in the *sccollab* buffer"
   "use #'current-time-string for a readable version, #'current-time"
   "for a more accurate one"
   :group 'sccollab
   :type 'lambda)

(defcustom sccollab-default-server-port 7777
  "default port to use for the sccollab local server"
  :group 'sccollab
  :type 'integer)

(defcustom sccollab-default-client-port 7777
  "default port to connect to on sccollab remote servers"
  :group 'sccollab
  :type 'integer)

(require 'osc)
(require 'sclang)

(defvar sccollab-debug t)
(defvar sccollab-server)
(defvar sccollab-clients)
(defvar sccollab-buffer)
(defvar sccollab-server-port sccollab-default-server-port)
(defvar sccollab-client-port sccollab-default-client-port)

(defmacro sccollab-entry (body)
  `(with-current-buffer sccollab-buffer
     (end-of-buffer)
     (insert ,body "//" (format "%s\n" (current-time)))))

(defun sccollab-server-start (&optional addr iface)
  "serve sccollab to people connecting at addrs"
  (interactive)
  (with-current-buffer (setq sccollab-buffer (get-buffer-create "*sccollab*"))
    (sclang-mode)
    (let ((interfaces (network-interface-list)))
      (unless (or addr iface)
      ;; here we are making a list of network devices, and prompting the user
	;; to select one, we then use the ip address of that device
	(setq iface
	      (let* (default-device-name (prompt "interface: ("))
		(let ((search-interfaces interfaces))
		  ;; find the first interface that is not loopback, or settle
		  ;; for loopback
		  (while (not default-device-name)
		    (if (not search-interfaces)
			(setq default-device-name "lo")
		      (if (not (string= (caar search-interfaces) "lo"))
			  (setq default-device-name (caar search-interfaces)))
		      (setq search-interfaces (cdr search-interfaces)))))
	      ;; create a prompt from all the interfaces
	      (dolist (d interfaces) (setq prompt
					   (format "%s %s" prompt (car d))))
	      (setq prompt (format "%s%s" prompt " ): "))
	      ;; get the ip address string from the interface definition
	      ;; if we end up overriding osc-make-server (good chance that we
	      ;; will have to, I think) just open the server by interface or
	      ;; device name.
	      (read-from-minibuffer prompt default-device-name))))
    (unless addr
      (setq addr (let ((ip (cdr (assoc-string iface interfaces))))
		   (if (not ip)
		       (error "could not open interface %s" iface))
		   (format "%d.%d.%d.%d"
			   (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3)))))
    (setq sccollab-server
	  (osc-make-server addr sccollab-server-port
			   (lambda (path &rest args)
			     (sccollab-receive path args)))))
    (end-of-buffer)
    (insert "// sccollab server started on " iface " " addr " port 7777 "
	    (format "%s\n" (current-time)))))


(defun sccollab-server-stop ()
  "stop the supercollider collaboration server"
  (interactive)
  (delete-process sccollab-server)
  (setq sccollab-server nil)
  (sccollab-entry "// stopped server "))
      
(defun sccollab-clients-stop ()
  "stop the supercollider collaboration clients"
  (interactive)
  (dolist (client sccollab-clients) (delete-process client))
  (setq sccollab-clients nil)
  (sccollab-entry "// disconnected from clients "))

(defun sccollab-stop ()
  "close all sccollab network connections"
  (interactive)
  (sccollab-server-stop)
  (sccollab-clients-stop))


(defun sccollab-send (path args)
  (when (stringp (car args))
      (when (and sclang-get-process (string= path "/sccollab/eval"))
	(sclang-eval-string (car args) t))
      (mapcar (lambda (client) 
		(apply 'osc-send-message
		       (cons client (cons path args))))
	      sccollab-clients)))

(defun sccollab-noeval-region (start end)
  "collaborate the current region, no evaluation"
  (interactive "r")
  (let ((to-send (buffer-substring start end)))
    (sccollab-entry to-send)
    (sccollab-send "/sccollab/noeval" to-send)))
  
(defun sccollab-eval-region (start end)
  "collaborate the current region, with evaluation"
  (interactive "r")
  (let ((to-send (buffer-substring start end)))
    (sccollab-entry to-send)
    (sclang-eval
    (sccollab-send "/sccollab/eval" to-send)))
  

(defun sccollab-receive (path args)
  (let ((remote (process-contact sccollab-server :remote)))
    (when (and (sclang-get-process) (string= path "sccollab/eval"))
      (sclang-eval-string (car args) t))
    (sccollab-entry (format "%s // from %d.%d.%d.%d:%d%s"
			    (if (stringp (car args)) (car args) "")
			    (aref remote 0) (aref remote 1) (aref remote 2)
			    (aref remote 3) (aref remote 4)
			    (if sccollab-debug
				(format " osc path: <%s> args: <%s> "
					path args) "")))))

(defun sccollab (&optional ip-list)
  "start sharing with a set of collaborators"
  (interactive)
  (with-current-buffer (setq sccollab-buffer (get-buffer-create "*sccollab*"))
    (sclang-mode)
    (unless ip-list
      (let (new-ip)
	(while (> (length
		   (setq new-ip (read-from-minibuffer
				 (format "%s add ip: "
					 (if ip-list ip-list "")))))
		  0)
	  (setq ip-list (cons new-ip ip-list)))))
    (dolist (server-ip ip-list)
      (setq sccollab-servers (cons (osc-make-client server-ip
						    sccollab-server-port)
				   sccollab-servers))
      (end-of-buffer)
      (insert "// connected to " server-ip " " sccollab-client-port " "
	      (format "%s\n" (current-time))))))
