(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-hook 'lisp-mode-hook #'slime-mode)
(setq slime-complete-symbol-function #'slime-fuzzy-complete-symbol)

(provide 'setup-slime)
