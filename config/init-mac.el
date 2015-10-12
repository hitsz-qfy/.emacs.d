; (setq mac-option-modifier 'super)
; (setq mac-command-modifier 'meta)
; (setq ns-function-modifier 'hyper)

(setq mac-control-modifier 'control)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'alt)


(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; Use aspell for spell checking: brew install aspell --lang=en
(setq ispell-program-name "/usr/local/bin/aspell")

(provide 'init-mac)
