(require 'osc)

(defvar sccollab-server)
(defvar sccollab-clients)

(defun sccollab-server-start (&optional addr)
  "serve sccollab to people connecting at addrs"
  (interactive)
  (unless addr
    ;; here we are making a list of network devices, and prompting the user
    ;; to select one, we then use the ip address of that device
    (setq addr
	  (let* (find-default-device default-device-name
				     (interfaces (network-interface-list))
				     (prompt "device: ("))
	    (let ((search-interfaces interfaces))
	      ;; find the first interface that is not loopback, or settle for
	      ;; loopback
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
	    (let* ((chosen (read-from-minibuffer prompt default-device-name))
		   (ip (cdr (assoc-string chosen interfaces))))
	      (if (not ip)
		  (error "could not open interface %s" chosen))
	      (format "%d.%d.%d.%d"
		      (aref ip 0) (aref ip 1) (aref ip 2) (aref ip 3))))))
	  (setq sccollab-server
		(osc-make-server addr 7777 
				 (lambda (path &rest args)
				   (sccollab-receive path args)))))

(defun sccollab-server-stop ()
  "stop the supercollider collaboration server"
  (interactive)
  (delete-process sccollab-server)
  (setq sccollab-server nil))

(defun sccollab-clients-stop ()
  "stop the supercollider collaboration clients"
  (interactive)
  (dolist (client sccollab-clients) (delete-process client))
  (setq sccollab-clients nil))

(defun sccollab-send (path args)
  (mapcar (lambda (client) 
            (apply 'osc-send-message
                   (cons client (cons path args))))
          sccollab-clients))

(defun sccollab-receive (path args)
  (let ((remote (process-contact sccollab-server :remote)))
  (display-warning
   :debug
   (format "received from %d.%d.%d.%d via osc path: %s args: %s"
	   (aref remote 0) (aref remote 1) (aref remote 2) (aref remote 3)
			   path args))))

(defun sccollab (&optional ip-list)
  "start sharing with a set of collaborators"
  (interactive)
  (unless ip-list
    (let (new-ip)
      (while (> (length
		 (setq new-ip (read-from-minibuffer
			       (format "%s add ip: " (if ip-list ip-list "")))))
		       0)
	(setq ip-list (cons new-ip ip-list)))))
  (dolist (client-ip ip-list)
	  (setq sccollab-clients (cons (osc-make-client client-ip 7777)
				       sccollab-clients))))

