(require 'osc)

(let* ((sccollab-server (osc-make-server "localhost" 7777))
       sccollab-clients
       get-ip)
  (setq get-ip (arg ip)
	"get an ip address"
	(interactive "s\add ip: "))
  (defun sccollab (arg ip-list) 
    "start a supercollider collaboration"
    (interactive)
    (unless ip-list
      (let (new-ip)
	(while (setq new-ip (get-ip))
	  (setq ip-list (cons new-ip ip-list)))))
    (setq sccollab-clients
	  (mapcar (lambda (client-ip) (osc-make-client client-ip 7777))
		  ip-list))))
