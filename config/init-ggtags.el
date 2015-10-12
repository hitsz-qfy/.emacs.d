(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-]") ' ggtags-find-tag-dwim)
(define-key ggtags-mode-map (kbd "C-t") 'pop-tag-mark)
(define-key ggtags-mode-map (kbd "M-r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(provide 'init-ggtags)
