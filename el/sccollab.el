(require 'osc)

(defvar sccollab-server)
(defvar sccollab-clients)

(defun sccollab-server-start ()
  (interactive)
  (setq sccollab-server
	(osc-make-server "local" 7777 
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
  (display-warning :debug "received from %s" path))

(defun sccollab (&optional ip-list)
  "start a supercollider collaboration"
  (interactive)
  (unless ip-list
    (let (new-ip)
      (while (> (length (setq new-ip (read-from-minibuffer "add ip: "))) 0)
	(setq ip-list (cons new-ip ip-list))
	(display-warning :debug "added ip %s" new-ip))))
  (setq sccollab-clients
	(mapcar (lambda (client-ip) (osc-make-client client-ip 7777))
		ip-list)))



