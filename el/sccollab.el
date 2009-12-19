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
  "stop the supercollider collaboration"
  (interactive)
  (delete-process sccollab-server)
  (setq sccollab-server nil))

(defun sccollab-send (path args)
  (mapcar (lambda (client) 
            (apply 'osc-send-message
                   (cons client (cons path args))))
          sccollab-clients))

(defun sccollab-receive (path args)
  (display-warning :debug
		   (format "received from osc path: %s args: %s" path args)))

(defun sccollab (&optional ip-list)
  "start a supercollider collaboration"
  (interactive)
  (unless ip-list
    (let (new-ip)
      (while (> (length (setq new-ip (read-from-minibuffer "add ip: "))) 0)
	(setq ip-list (cons new-ip ip-list))
	(display-warning :debug (format "added ip %s" new-ip)))))
  (setq sccollab-clients
	(mapcar (lambda (client-ip) (osc-make-client client-ip 7777))
		ip-list)))

