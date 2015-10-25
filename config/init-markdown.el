(require 'livedown)

(custom-set-variables
 '(livedown:autostart nil) ; automatically open preview when opening markdown files 
 '(livedown:open t)        ; automatically open the browser window
 '(livedown:port 1337))    ; port for livedown server

(global-set-key (kbd "C-M-m") 'livedown:preview)

(provide 'init-markdown)