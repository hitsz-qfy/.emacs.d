(defun get-erl-man ()
  (interactive)
  (let* ((man-path "/usr/local/lib/erlang/man")
         (man-args (format "-M %s %s" man-path (current-word))))
    (man man-args)))

;;(global-set-key [(f5)] (lambda () (interactive) (get-erl-man)))
(global-set-key (kbd "C-x w") (lambda () (interactive) (get-erl-man)))



(provide 'init-man)
