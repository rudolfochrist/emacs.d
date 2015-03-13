(require-package 'multi-term)

(setq multi-term-program "/bin/zsh")

(global-set-key (kbd "<f1>") 'multi-term)

(add-to-list 'term-bind-key-alist '("C-c C-n" . multi-term-next))
(add-to-list 'term-bind-key-alist '("C-c C-p" . multi-term-prev))
(add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
(add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))

(provide 'init-term)
