(require-package 'sotlisp)

(sotlisp-define-function-abbrev "rp" "require-package '")

(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(speed-of-thought-mode 1)

(provide 'init-sotlisp)
