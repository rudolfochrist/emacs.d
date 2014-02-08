(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'lisp-interaction-mode-hook #'slime-mode)

(provide 'setup-slime)
