(require 'git-gutter)

(global-git-gutter-mode t)
(git-gutter:linum-setup)


;; (setq git-gutter:window-width 2)
;; (setq git-gutter:modified-sign "⇔")
;; (setq git-gutter:added-sign "⇒")
;; (setq git-gutter:deleted-sign "⇐")


(setq git-gutter:window-width 2)
(custom-set-variables
;; '(git-gutter:modified-sign "  ") ;; two space
 '(git-gutter:modified-sign "==")
 '(git-gutter:added-sign "++")    ;; multiple character is OK
 '(git-gutter:deleted-sign "--"))

 (set-face-background 'git-gutter:modified "purple") ;; background color
 ;;(set-face-foreground 'git-gutter:added "green")
 ;;(set-face-foreground 'git-gutter:deleted "red")


(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)

(custom-set-variables
 '(git-gutter:handled-backends '(git hg bzr)))

(require 'smartrep)
(smartrep-define-key
    global-map  "C-x" '(("p" . 'git-gutter:previous-diff)
                        ("n" . 'git-gutter:next-diff)))

;; {{ git-messenger
(autoload 'git-messenger:popup-message "git-messenger" "" t)
;; show details to play `git blame' game
(setq git-messenger:show-detail t)
  (add-hook 'git-messenger:after-popup-hook
          (lambda (msg)
            ;; extract commit id and put into the kill ring
            (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
              (copy-yank-str (match-string 2 msg))
              (message "commit hash => clipboard & kill-ring")
              )))
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
;; }}


(provide 'init-git-gutter)
