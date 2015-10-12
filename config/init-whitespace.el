
;; whitespace
(require 'whitespace)
(unless (member 'whitespace-mode prog-mode-hook)
  (add-hook 'prog-mode-hook 'whitespace-mode))
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(set-default 'indicate-empty-lines t)
(set-default 'indent-tabs-mode nil)
(setq whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80)

(defun seancribbs/untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun seancribbs/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun seancribbs/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (seancribbs/indent-buffer)
  (seancribbs/untabify-buffer)
  (delete-trailing-whitespace))

(global-set-key (kbd "C-c n") 'seancribbs/cleanup-buffer)


(provide 'init-whitespace)
