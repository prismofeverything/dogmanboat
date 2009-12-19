(require 'osc)

(let* ((sccollab-server (osc-make-server "localhost" 7777))
       get-ip)
  (setq get-ip (arg ip)
	(interactive "s\add ip: "))
  (defun sccollab (arg iplist)
    (interactive)))
