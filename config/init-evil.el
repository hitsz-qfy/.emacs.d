;; evil-leader: Evil leader must be loaded before evil (as documented).
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
;;(evil-leader/set-leader "<SPC>")

(setq evil-leader/in-all-states 1)
(setq evil-move-cursor-back t)    ;; Move back the cursor one position when exiting insert mode

;; evil plugins
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-paredit)
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(require 'evil-nerd-commenter)
(evilnc-default-hotkeys)

(evil-leader/set-key
  ","  'projectile-find-file
  "."  'switch-to-previous-buffer
  "S"  'delete-trailing-whitespace
  "v"  'whitespace-mode                                                      ;; Show invisible characters
  "j"  'ace-jump-mode
  "d"  (lambda () (interactive) (evil-ex-call-command nil "bdelete" nil))
  "D"  'open-current-line-in-codebase-search
  "h"  'fontify-and-browse                                                   ;; HTML-ize the buffer and browse the result
  "w"  'save-buffer
  "k"  (lambda () (interactive) (get-erl-man))
  "s"  'ag-project
  "x"  'helm-M-x
  "y"  'yank-to-x-clipboard
  "B"  'magit-blame-mode
  "g"  'magit-status
  "l"  'windmove-left
  "r"  'windmove-right
  "o"  'other-window
  "er" 'evil-remove-too-much-space
  "nt" 'neotree-toggle
  "nn" 'narrow-and-set-normal                                                ;; Narrow to region and enter normal mode
  "nw" 'widen
  "ef" 'end-of-defun
  "mf" 'mark-defun
  "em" 'erase-message-buffer
  "fn" 'cp-filename-of-current-buffer
  "fp" 'cp-fullpath-of-current-buffer
  "eey" 'emmet-expand-yas
  "eel" 'emmet-expand-line
  "ut" 'undo-tree-visualize
  "aa" 'align-regexp
  ;"t"  'gtags-reindex
  ;"T"  'gtags-find-tag
  ;"gt" 'ggtags-find-tag-dwim
  ;"gr" 'ggtags-find-reference
  )

;; evil: Always use evil mode.
(evil-mode 1)

;; Global bindings
(global-set-key (kbd "C-r") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "C-S-P") 'helm-projectile-switch-project)
(define-key evil-normal-state-map (kbd "C-p")   'helm-projectile)
(define-key evil-normal-state-map (kbd "-")     'helm-find-files)
(define-key evil-normal-state-map (kbd "C-]")   'helm-gtags-find-tag)
(define-key evil-normal-state-map (kbd "C-t")   'helm-gtags-pop-stack)
(define-key evil-normal-state-map (kbd "C-c g r")   'helm-gtags-find-rtag)
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))


;; Make escape quit everything, whenever possible.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


;; Set cursor colors depending on mode
(when (display-graphic-p)
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  )

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))


;; 当 Emacs 进入 evil-insert 状态后，立刻进入 evil-emacs 状态
;; 当我处于 evil-emacs 状态时，可以通过 C-[ 直接进入 evil-normal 状态
;; 当我处于 evil-visual 状态时，按 i 可以直接进入 evil-emacs 状态
(add-hook 'evil-insert-state-entry-hook 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "C-[") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "i") 'evil-emacs-state)

(loop for (mode . state) in
      '((minibuffer-inactive-mode . emacs)
        (ggtags-global-mode . emacs)
        (grep-mode . emacs)
        (Info-mode . emacs)
        (term-mode . emacs)
        (direx:direx-mode . emacs)
        (neotree-mode . emacs)
        (help-mode . emacs)
        (eshell-mode . emacs)
        (shell-mode . emacs)
        (fundamental-mode . emacs)
        (dired-mode . emacs)
        (compilation-mode . emacs)
        (js2-error-buffer-mode . emacs)
        ;(magit-log-edit-mode . emacs)
        ;(magit-commit-mode . normal)
        ;(magit-diff-mode . normal)
        )
      do (evil-set-initial-state mode state))

;; Finally, there are some modes that I want to always be in Emacs mode instead of Evil.
(setq evil-emacs-state-modes
      '(archive-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode completion-list-mode ediff-mode ediff-meta-mode efs-mode etags-select-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode ibuffer-mode magit-key-mode magit-log-mode magit-mode magit-reflog-mode magit-show-branches-mode magit-branch-manager-mode magit-stash-mode magit-status-mode magit-wazzup-mode org-agenda-mode package-menu-mode))

(provide 'init-evil)
