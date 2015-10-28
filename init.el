(setq user-full-name "Xuancong Lee")
(setq user-mail-address "robertzhouxh@gmail.com")		+(setq user-mail-address "congleetea@gmail.com")
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/usr/bin")))
(setq load-path (cons "/usr/local/lib/gtags" load-path))

;; What OS/window system am I using?
(setq is-mac (equal system-type 'darwin))
;; Adapted from:
;; https://github.com/purcell/emacs.d/blob/master/init.el
(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Is this running on OS X?")

(defconst *is-carbon-emacs*
  (and *is-a-mac* (eq window-system 'mac))
  "Is this the Carbon port of Emacs?")

(defconst *is-cocoa-emacs*
  (and *is-a-mac* (eq window-system 'ns))
  "Is this the Cocoa version of Emacs?")

(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")

(setenv "GOPATH" (concat (getenv "HOME") "goEnv"))

;; things that don't come from package managers
(defvar robertzhouxh/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(defvar robertzhouxh/config-dir (expand-file-name "config" user-emacs-directory))

(add-to-list 'load-path robertzhouxh/vendor-dir)
(add-to-list 'load-path robertzhouxh/config-dir)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(dolist (project (directory-files robertzhouxh/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 'cl)
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;;; Pin some packages to specific repositories.
(setq package-pinned-packages '((gtags . "marmalade")
                                (magit . "melpa-stable")))

(defvar robertzhouxh/packages '(dash                          ;A modern list api for Emacs. No 'cl required
                                ack                           ;do grep
                                ag                            ;do grep
                                expand-region                 ;smart region selection
                                ffap                          ;find file at point
                                projectile                    ;find file/folder in project
                                restclient                    ;restfull api test tool
                                flx                           ;fuzzy matching
                                flx-ido                       ;fuzzy matching for ido
                                ido-ubiquitous                ;use ido nearly everywhere
                                ido-vertical-mode
                                smex
                                fullframe
                                dash-at-point
                                diminish
                                tabbar
                                multiple-cursors
                                smartrep
                                golden-ratio
                                switch-window
                                undo-tree
                                smartparens
                                guide-key
                                whitespace
                                whitespace-cleanup-mode
                                smart-mode-line
                                rainbow-delimiters
                                neotree
                                paredit
                                htmlize
                                popwin

                                flycheck
                                flycheck-pos-tip
                                flycheck-clojure

                                company
                                company-c-headers
                                yasnippet

                                ggtags
                                helm-gtags
                                helm-projectile

                                ;; evil
                                evil-leader
                                evil-surround
                                evil-paredit
                                evil-nerd-commenter
                                evil-matchit
                                evil

                                ;; modes
                                lua-mode
                                python-mode
                                markdown-mode
                                cmake-mode
                                json-mode
                                web-mode
                                sass-mode
                                less-css-mode
                                coffee-mode
                                js2-mode
                                emmet-mode
                                protobuf-mode
                                dockerfile-mode
                                rainbow-mode
                                ace-jump-mode
                                avy


                                ;; Dired
                                dired-details
                                dired-details+
                                dired+
                                dired-rainbow

                                ;; UI
                                monokai-theme
                                sublime-themes
                                zenburn-theme
                                solarized-theme
                                spacegray-theme
                                tangotango-theme

                                ;; GIT
                                git-gutter
                                git-messenger
                                magit
                                ))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;;
;; (dolist (p robertzhouxh/packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))
(defun robertzhouxh/packages-installed-p ()
  (loop for pkg in robertzhouxh/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (robertzhouxh/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg robertzhouxh/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;This sets $MANPATH, $PATH and exec-path from your shell, but only on OS X.
;;(exec-path-from-shell-copy-env "PYTHONPATH")

(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(require 'init-util)
(require 'init-bootstrap)
(require 'helm-projectile)
(require 'init-mac)
(require 'init-mode-mapps)
(require 'init-tabbar)
(require 'init-autoinsert)
(require 'init-ibuffer)
(require 'init-yasnippet)
(require 'init-whitespace)
(require 'init-undo-tree)
(require 'init-projejctile)
(require 'init-mutiple-curesor)
(require 'init-magit)
(require 'init-git-gutter)
(require 'init-ido)
(require 'init-dash-at-point)
(require 'init-company)
(require 'init-ack)
(require 'init-ace-jump-mode)
(require 'init-expand-region)
(require 'init-flycheck)
(require 'init-flyspell)
(require 'init-guide-key)
;;(require 'init-ggtags)
(require 'init-helm-gtags)
(require 'dired-sort-map)
(require 'init-dired)
(require 'init-clipboard)
(require 'init-keybindings)
(require 'init-neotree)
(require 'init-markdown)
(require 'init-misc-modes)
(require 'init-org-mode)
(require 'init-cc-mode)
(require 'init-erlang)
(require 'init-evil)
(require 'init-popwin)
(require 'init-man)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "30b7087fdd149a523aa614568dc6bacfab884145f4a67d64c80d6011d4c90837" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" default)))
 '(doc-view-continuous nil)
 '(git-gutter:added-sign "++")
 '(git-gutter:deleted-sign "--")
 '(git-gutter:handled-backends (quote (git hg bzr)))
 '(git-gutter:modified-sign "==")
 '(livedown:autostart nil)
 '(livedown:open t)
 '(livedown:port 1337)
 '(package-selected-packages
   (quote
    (htmlize neotree zenburn-theme yasnippet whitespace-cleanup-mode web-mode tangotango-theme tabbar switch-window sublime-themes spacegray-theme solarized-theme smex smartrep smartparens smart-mode-line sass-mode restclient rainbow-mode rainbow-delimiters python-mode protobuf-mode powerline multiple-cursors monokai-theme markdown-mode magit lua-mode less-css-mode json-mode js2-mode ido-vertical-mode ido-ubiquitous helm-projectile helm-gtags guide-key golden-ratio git-messenger git-gutter ggtags fullframe flycheck-pos-tip flycheck-clojure flx-ido expand-region evil-surround evil-paredit evil-nerd-commenter evil-matchit evil-leader emmet-mode dockerfile-mode dired-rainbow dired-details+ dired+ diminish dash-at-point company-c-headers coffee-mode cmake-mode ag ack ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(neo-banner-face ((t :inherit shadow)) t)
 '(neo-button-face ((t :inherit dired-directory)) t)
 '(neo-dir-link-face ((t :inherit dired-directory)) t)
 '(neo-expand-btn-face ((t :inherit button)) t)
 '(neo-file-link-face ((t :inherit default)) t)
 '(neo-header-face ((t :inherit shadow)) t)
 '(neo-root-dir-face ((t :inherit link-visited :underline nil)) t))
