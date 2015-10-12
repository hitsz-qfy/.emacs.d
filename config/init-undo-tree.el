
;; Package: undo-tree
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "C-x C-u") 'undo-tree-redo)


(provide 'init-undo-tree)
