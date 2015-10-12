
;; mutiple-curesor
(require 'multiple-cursors)
; refer ===> http://sheephead.homelinux.org/2011/12/19/6930/
; 先mark: 先选中region 然后act c-t i or ...
;(require 'smartrep)
;(declare-function smartrep-define-key "smartrep")
;(global-set-key (kbd "C-M-c") 'mc/edit-lines)
;(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
;(global-unset-key "\C-t")
;(smartrep-define-key global-map "C-t"
;  '(("C-t"      . 'mc/mark-next-like-this)
;    ("n"        . 'mc/mark-next-like-this)
;    ("p"        . 'mc/mark-previous-like-this)
;    ("m"        . 'mc/mark-more-like-this-extended)
;    ("u"        . 'mc/unmark-next-like-this)
;    ("U"        . 'mc/unmark-previous-like-this)
;    ("s"        . 'mc/skip-to-next-like-this)
;    ("S"        . 'mc/skip-to-previous-like-this)
;    ("*"        . 'mc/mark-all-like-this)
;    ("d"        . 'mc/mark-all-like-this-dwim) ;on html tag name
;    ("i"        . 'mc/insert-numbers)
;    ("o"        . 'mc/sort-regions)
;    ("O"        . 'mc/reverse-regions)))

;;(global-set-key (kbd "C->") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-next-like-this)

(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m *") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m s") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c m S") 'mc/skip-to-previous-like-this)


;; From active region to multiple cursors:
(global-set-key (kbd "C-c m r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)



(provide 'init-mutiple-curesor)
