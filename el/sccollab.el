(require 'osc)

(let* ((sccollab-server (osc-make-server "localhost" 7777))
       sccollab-clients
       get-ip)
  (setq get-ip (arg ip)
	(interactive "s\add ip: "))
  (defun sccollab (arg ip-list) 
    "start a supercollider collaboration"
    (interactive)
    (setq sccollab-clients 
	  (mapcar (lambda (client-ip) (osc-make-client client-ip 7777))
		  ip-list))))
