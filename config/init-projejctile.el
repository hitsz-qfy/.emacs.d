;; projejctile.el & ag.el
;;[C-c p f] Find a file in the project.
;;[C-c p l] Find a file in a directory.
;;[C-c p g] Find text matches in the project
;;   ref    ===> http://emacs.stackexchange.com/questions/448/what-is-the-easiest-way-to-search-all-useful-files-inside-a-single-project
;;   ag.el  ===> http://agel.readthedocs.org/en/latest/index.html
;;               Projectile supports ag.el ===> C-c p s s runs ag-regexp on your project.
(require 'projectile)
(require 'ag)
(setq ag-highlight-search t)
;;(setq projectile-keymap-prefix (kbd "C-c C-p"))

(setq projectile-indexing-method 'git)
(setq projectile-enable-caching t)
(projectile-global-mode t)


(provide 'init-projejctile)

