
;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
;;(delete 'company-semantic company-backends);for c-clang-complete
;(global-company-mode 1)
(setq company-show-numbers t)
(setq company-idle-delay 0.1)           ;延迟时间
(setq company-minimum-prefix-length 2)  ;触发补全的字符数量
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-downcase nil)
(setq company-backends (remove 'company-semantic company-backends))
(setq company-backends (remove 'company-eclim company-backends))
(setq company-backends (remove 'company-oddmuse company-backends))
(setq company-backends (remove 'company-etags company-backends))
(setq company-backends (remove 'company-gtags company-backends))
(setq company-backends (remove 'company-xcode company-backends))
(setq company-backends (remove 'company-clang company-backends))
(setq company-backends (remove 'company-cmake company-backends))
(add-to-list 'company-backends 'company-c-headers)

(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
  (progn (company-abort)
         (yas/expand))
    (company-complete-common)))

(defun yas/expansion-at-point ()
  (first (yas--current-key)))

(define-key company-active-map (kbd "TAB") 'company-yasnippet-or-completion)
(define-key company-active-map (kbd "<tab>") 'company-yasnippet-or-completion)



(provide 'init-company)
