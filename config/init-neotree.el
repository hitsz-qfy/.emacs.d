;(setq neo-theme 'ascii)
(setq neo-theme 'arrow)
(custom-set-faces
 '(neo-banner-face ((t . (:inherit shadow))) t)
 '(neo-header-face ((t . (:inherit shadow))) t)
 '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
 '(neo-dir-link-face ((t . (:inherit dired-directory))) t)
 '(neo-file-link-face ((t . (:inherit default))) t)
 '(neo-button-face ((t . (:inherit dired-directory))) t)
 '(neo-expand-btn-face ((t . (:inherit button))) t))

  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
              (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(provide 'init-neotree)