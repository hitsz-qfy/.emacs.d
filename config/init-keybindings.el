(global-set-key [f4] 'eshell)
(global-set-key (kbd "C-x M-d") 'insert-current_time)

;; global keybindings
(global-set-key (kbd  "C-c G") 'search-github)
(global-set-key (kbd  "C-c s") 'search-google)
(global-set-key (kbd  "C-c q") 'search-code)

(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-\\") 'dabbrev-expand)

(global-set-key (kbd "M-]") 'next-buffer)
(global-set-key (kbd "M-[") 'previous-buffer)

(global-set-key (kbd "C-x C-k") 'kill-current-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)

(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c C-v") 'browse-url)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-u") 'kill-back-to-indentation)
(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)
(global-set-key (kbd "C-c o s") 'sudo-reopen-file)


;; 连续滚屏
(defun hold-line-scroll-up()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (let ((next-screen-context-lines
         (count-lines
          (window-start) (window-end))))
    (scroll-up)))

(defun hold-line-scroll-down()
  "Scroll the page with the cursor in the same line"
  (interactive)
  (let ((next-screen-context-lines
        (count-lines
        (window-start) (window-end))))
    (scroll-down)))

(define-key global-map (kbd "M-n") 'scroll-other-window)
(define-key global-map (kbd "M-p") 'scroll-other-window-down)
;;(global-set-key (kbd "M-n") 'scroll-up-line)
;;(global-set-key (kbd "M-p") 'scroll-down-line)
;;(global-set-key (kbd "C-n") 'hold-line-scroll-up)
;;(global-set-key (kbd "C-p") 'hold-line-scroll-down)


(provide 'init-keybindings)
