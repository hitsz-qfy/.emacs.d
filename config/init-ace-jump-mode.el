;; ace-jump-mode共有3个子模式，char, word和line,默认是word，加上前缀C-u是char,两个前缀C-u C-u是line.
;; word模式只匹配以输入字符开头的单词，line模式在非空行行首显示字符以便跳转到想要到的行。

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(provide 'init-ace-jump-mode)
