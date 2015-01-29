(require-package 'eval-sexp-fu)

(add-hook 'lisp-mode-hook #'(lambda ()
                              (eval-sexp-fu-flash-mode 1)))

(provide 'init-eval-sexp-fu)
