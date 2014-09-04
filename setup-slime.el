(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(add-hook 'lisp-mode-hook #'slime-mode)
(setq slime-complete-symbol-function #'slime-fuzzy-complete-symbol)

;;; use UTF-8
(setq slime-net-coding-system 'utf-8-unix)

(provide 'setup-slime)
