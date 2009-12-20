(require 'osc)

(defvar sccollab-debug t)
(defvar sccollab-server)
(defvar sccollab-clients)
(defvar sccollab-buffer)

(defmacro sccollab-entry (body)
  `(with-current-buffer sccollab-buffer
     (end-of-buffer)
     (insert ,body "//" (current-time-string) "\n")))

(defun sccollab-server-start (&optional addr iface)
  "serve sccollab to people connecting at addrs"
  (interactive)
  (with-current-buffer (setq sccollab-buffer (get-buffer-create "*sccollab*"))
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
	  (osc-make-server addr 7777 
			   (lambda (path &rest args)
			     (sccollab-receive path args)))))
    (end-of-buffer)
    (insert "// sccollab server started on " iface " " addr " port 7777 "
	    (current-time-string) "\n")))


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

(defun sccollab-send (path args)
  (mapcar (lambda (client) 
            (apply 'osc-send-message
                   (cons client (cons path args))))
          sccollab-clients))

(defun sccollab-receive (path args)
  (let ((remote (process-contact sccollab-server :remote)))
    (sccollab-entry (format "%s // from %d.%d.%d.%d%s"
			    (if (stringp (car args)) (car args) "")
			    (aref remote 0) (aref remote 1) (aref remote 2)
			    (aref remote 3)
			    (if sccollab-debug
				(format " osc path: <%s> args: <%s> "
					path args) "")))))

(defun sccollab (&optional ip-list)
  "start sharing with a set of collaborators"
  (interactive)
  (with-current-buffer (setq sccollab-buffer (get-buffer-create "*sccollab*"))
    (unless ip-list
      (let (new-ip)
	(while (> (length
		   (setq new-ip (read-from-minibuffer
				 (format "%s add ip: "
					 (if ip-list ip-list "")))))
		  0)
	  (setq ip-list (cons new-ip ip-list)))))
    (dolist (client-ip ip-list)
      (setq sccollab-clients (cons (osc-make-client client-ip 7777)
				   sccollab-clients))
      (end-of-buffer)
      (insert "// connected to " client-ip " 7777 "
	      (current-time-string)"\n"))))
