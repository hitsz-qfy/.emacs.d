
(when is-mac
  (defun paste-with-Xclipboard ()
	(shell-command-to-string "pbpaste"))

  (defun copy-to-Xclipboard (text &optional push)
	(let ((process-connection-type nil))
	  (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
		(process-send-string proc text)
		(process-send-eof proc)))))

(unless is-mac
  (defun paste-with-Xclipboard ()
	(shell-command-to-string "xsel -ob"))

  (defun copy-to-Xclipboard (text &optional push)
	(let ((process-connection-type nil))
	  (let ((proc (start-process "copy_to_X" "*Messages*" "xsel" "-ib")))
		(process-send-string proc text)
		(process-send-eof proc)))))

(setq interprogram-cut-function 'copy-to-Xclipboard)
(setq interprogram-paste-function 'paste-with-Xclipboard)

(provide 'init-clipboard)
