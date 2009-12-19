(require 'osc)


(let* (sccollab-server
       sccollab-clients
       get-ip)
  (setq get-ip (arg ip)
	(interactive "s\add ip: "))
  (defun start-sccollab-server ()
    (setq sccollab-server (osc-make-server "localhost" 7777 
                                           (lambda (path &rest args)
                                             (receive-collaboration path args)))))
  (defun sccollab (arg ip-list)
    "start a supercollider collaboration"
    (interactive)
    (setq sccollab-clients 
	  (mapcar (lambda (client-ip) (osc-make-client client-ip 7777))
		  ip-list))))
