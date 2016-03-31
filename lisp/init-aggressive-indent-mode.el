(require-package 'aggressive-indent)
(global-aggressive-indent-mode 1)

(dolist (mode '(slime-repl-mode asm-mode))
  (add-to-list 'aggressive-indent-excluded-modes mode))

(provide 'init-aggressive-indent-mode)
