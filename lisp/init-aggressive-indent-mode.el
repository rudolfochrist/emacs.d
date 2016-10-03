(require-package 'aggressive-indent)

(global-aggressive-indent-mode 1)

(add-to-list 'aggressive-indent-excluded-modes 'slime-repl-mode)
(add-to-list 'aggressive-indent-excluded-modes 'feature-mode)

(provide 'init-aggressive-indent-mode)
