
;; magit
(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)
(setq magit-git-executable "/usr/local/bin/git")
(global-set-key (kbd "C-c g t") 'magit-status)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (when (get-register :magit-fullscreen)
    (jump-to-register :magit-fullscreen)))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(setq diff-switches "-u")


(provide 'init-magit)
