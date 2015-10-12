
;; ack
(require 'ack)
(defun ack-dired (dir pattern)
  "Makes `ack` behave more like `find-grep-dired`"
  (interactive "DAck (directory): \nsAck: (pattern): ")
  (ack (concat ack-command " " pattern) dir))
;; (global-set-key (kbd "C-x C-g") 'ack-dired) ;; Bind C-x C-g to ack-dired


(provide 'init-ack)
