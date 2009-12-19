(require 'osc)

(defvar sccollab-server)
(defvar sccollab-clients)

(defun sccollab-server-start (&optional addr)
  "serve sccollab to people connecting at addr"
  (interactive "Mip to serve from: ") ; how to get ip of default address?
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



