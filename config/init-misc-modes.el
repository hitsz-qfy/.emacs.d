;;; markdown-mode
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (writegood-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
(setq markdown-css-paths (expand-file-name "markdown.css" robertzhouxh/vendor-dir))


;;; sh-mode:
(add-hook 'sh-mode-hook (lambda ()
                         (setq sh-basic-offset 2)
                         (setq sh-indentation 2)))


;;; emmet-mode

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)


;;; 找到#!声明,自动給script文件加上可执行文件
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(provide 'init-misc-modes)
