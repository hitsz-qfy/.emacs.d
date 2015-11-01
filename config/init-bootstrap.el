(require 'dash)

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(winner-mode 1)                                  ;; Undo/redo window configuration with C-c <left>/<right>
(auto-compression-mode t)                        ;; Transparently open compressed files
(delete-selection-mode 1)                        ;; Remove text in active region if inserting text
(global-auto-revert-mode 1)                      ;; Auto refresh buffers
(global-hl-line-mode 1)                          ;; Highlight current line

(setq inhibit-startup-message t)                 ;; No splash screen please ... jeez
(setq global-auto-revert-non-file-buffers t)     ;; Also auto refresh dired, but be quiet about it
(setq auto-revert-verbose nil)
(setq echo-keystrokes 0.1)                       ;; Show keystrokes in progress
(setq delete-by-moving-to-trash t)               ;; Move files to trash when deleting
(setq shift-select-mode nil)                     ;; Real emacs knights don't use shift to mark things
(setq jump-char-lazy-highlight-face nil)         ;; Don't highlight matches with jump-char - it's distracting
(setq enable-recursive-minibuffers t)            ;; Allow recursive minibuffers
(setq gc-cons-threshold 20000000)                ;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq org-replace-disputed-keys t)               ;; org-mode: Don't S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq electric-indent-mode nil)                  ;; No electric indent
(setq eval-expression-print-level nil)           ;; Nic says needs to be nil (turned off) so that you can always see what's happening.
(setq org-src-fontify-natively t)                ;; Fontify org-mode code blocks
(setq large-file-warning-threshold 100000000)    ;; Warn only when opening files bigger than 100MB
(setq ns-pop-up-frames nil)                      ;; Don't open files from the workspace in a new frame
;;(setq default-directory "~/githubs") 
;;(setq command-line-default-directory "~/githubs")
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

;; no buffers when starting
(setq initial-scratch-message "")    ;; Makes *scratch* empty

;;Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

(set-default 'imenu-auto-rescan t)
(set-default 'indent-tabs-mode nil)              ;; Never insert tabs
(set-default 'indicate-empty-lines t)            ;; Show me empty lines after buffer end
(set-default 'sentence-end-double-space nil)     ;; Sentences do not need double spaces to end. Period.
(setq-default truncate-lines t)                  ;; Don't break lines for me, please
(defalias 'yes-or-no-p 'y-or-n-p)                ;; Answering just 'y' or 'n' will do

;; docview to see pdf
(custom-set-variables '(doc-view-continuous nil))

(require 'linum)
(global-linum-mode 1)
(setq line-number-mode t)                        ;; Always display line and column numbers
(setq column-number-mode t)
(setq linum-format "%3d ")
(setq fill-column 80)                            ;; Lines should be 80 characters wide, not 72

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 100)

;; recentf-mode
(recentf-mode 1)                                 ;; Save a list of recent files visited. (open recent file with C-x f)
(setq recentf-max-saved-items 100)               ;; just 20 is too recent

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")
(set-foreground-color "grey")
(set-background-color "black")
(set-cursor-color "Orchid")
(set-mouse-color "gold1")
(set-scroll-bar-mode nil)
;(setq-default cursor-type 'bar)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

(require 'cl-lib)

(add-hook 'doc-view-mode-hook #'auto-revert-mode)

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))
(eval-after-load "subword" '(diminish 'subword-mode))

;; Rainbow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Default setup of smartparens
;; smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

(require 'ffap)
(require 'restclient)
;; C-c C-c: runs the query under the cursor, tries to pretty-print the response (if possible)
;; C-c C-r: same, but doesn't do anything with the response, just shows the buffer
;; C-c C-v: same as C-c C-c, but doesn't switch focus to other window
;; C-c C-p: jump to the previous query
;; C-c C-n: jump to the next query
;; C-c C-.: mark the query under the cursor
;; C-c C-u: copy query under the cursor as a curl command

;;(require 'mmm-mode)
;;(setq mmm-global-mode 'maybe)


;; Hidden minor-mode, by rich-minority substitue for the powerline
(setq rm-excluded-modes
    '(" Guide"            ;; guide-key mode
      " hc"               ;; hardcore mode
      " vl"               ;; global visual line mode enabled
      " Wrap"             ;; shows up if visual-line-mode is enabled for that buffer
      " Omit"             ;; omit mode in dired
      " drag"             ;; drag-stuff-mode
      " VHl"              ;; volatile highlights
      " ctagsU"           ;; ctags update
      " Undo-Tree"        ;; undo tree
      " wr"               ;; Wrap Region
      " SliNav"           ;; elisp-slime-nav
      " Fly"              ;; Flycheck
      " PgLn"             ;; page-line-break
      " GG"               ;; ggtags
      " ElDoc"            ;; eldoc
      " hl-highlight"     ;; hl-anything
    ))

(require 'powerline)
;(powerline-default-theme)
(powerline-center-theme)
;(powerline-revert)
(when window-system
  (when (functionp 'set-fontset-font)
   (set-fontset-font "fontset-default"
                     'unicode
                     (font-spec :family "DejaVu Sans Mono"
                                :width 'normal
                                :size 12.4
                                :weight 'normal))))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "s-<down>") 'shrink-window)

(defun seancribbs/turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun seancribbs/add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'seancribbs/turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'seancribbs/add-watchwords)
(show-paren-mode t)

(load-theme 'monokai t)
(when window-system
  (load-theme 'solarized-dark t))

; 关闭启动提示错误
(setq ad-redefinition-action 'accept)

(provide 'init-bootstrap)
