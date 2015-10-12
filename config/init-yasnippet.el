
;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)
(setq yas/snippet-dirs (append '("~/.emacs.d/snippets/") yas/snippet-dirs))
;; automatic reload after snippets changed
(defun reload-yasnippets-on-save-snippets ()
  (when (string-match "/snippets/" buffer-file-name)
    (yas/reload-all)
    ))
(add-hook 'after-save-hook 'reload-yasnippets-on-save-snippets)


(provide 'init-yasnippet)
