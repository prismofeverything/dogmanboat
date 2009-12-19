(require 'osc)

(defvar sccollab-server)
(defvar sccollab-clients)
(defun receive-collaboration (path args)
  (display-warning :debug path "*Messages*"))
(defun sccollab-server-start ()
  (setq sccollab-server
	(osc-make-server "localhost" 7777 
			 (lambda (path &rest args)
			   (receive-collaboration path args)))))
(defun sccollab-server-stop ()
  (delete-process sccollab-server)
  (setq sccollab-server nil))
(defun sccollab-send (path args)
  (mapcar (lambda (client) (apply 'osc-send-message (cons client (cons path args))))
          sccollab-clients))
(defun sccollab (&optional ip-list)
  "start a supercollider collaboration"
  (interactive)
  (unless ip-list
    (let (new-ip)
      (while (setq new-ip (read-from-minibuffer "add ip: "))
	(setq ip-list (cons new-ip ip-list))
	(display-warning :debug ip-list)))
  (setq sccollab-clients
	(mapcar (lambda (client-ip) (osc-make-client client-ip 7777))
		ip-list))))

