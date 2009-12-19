(require 'osc)

(let* ((sccollab-server (osc-make-server "localhost" 7777))
       sccollab-clients)
  (defun sccollab (iplist) 
    (setq sccollab-clients 
          (mapcar (lambda (client-ip) (osc-make-client client-ip 7777))))))




