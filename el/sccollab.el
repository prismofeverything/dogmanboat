(require 'osc)

(defvar sccollab-server)
(defvar sccollab-clients)
(defun start-sccollab-server ()
  (setq sccollab-server
	(osc-make-server "localhost" 7777 
			 (lambda (path &rest args)
			   (receive-collaboration path args)))))
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