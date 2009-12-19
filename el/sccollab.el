(require 'osc)


(let* (sccollab-server
       sccollab-clients
       path-listeners
       get-ip)
  (setq get-ip (arg ip)
	"get an ip address"
	(interactive "s\add ip: "))
  (defun start-sccollab-server ()
    (setq sccollab-server (osc-make-server "localhost" 7777 
                                           (lambda (path &rest args)
                                             (receive-collaboration path args)))))
  (defun receive-collaboration (path args)
    (display-warning :debug path "*Messages*"))
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
