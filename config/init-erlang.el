(let* ((emacs-version "2.8.1")
       (tools-path
        (concat "/usr/local/lib/erlang/lib/tools-" emacs-version "/emacs")))
  (when (file-exists-p tools-path)
    (setq load-path (cons tools-path load-path))
    (setq erlang-root-dir "/usr/local/lib/erlang")
    (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
    (require 'erlang-start)
    (defvar inferior-erlang-prompt-timeout t)))

(defun erlang-insert-binary ()
  "Inserts a binary string into an Erlang buffer and places the
  point between the quotes."
  (interactive)
  (insert "<<\"\">>")
  (backward-char 3)
  )

(eval-after-load "erlang" '(define-key erlang-mode-map (kbd "C-c b") 'erlang-insert-binary))
(add-hook 'erlang-mode-hook 'whitespace-mode)

(provide 'init-erlang)
