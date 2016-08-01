(let ((byte-compile-not-obsolete-funcs
       (append byte-compile-not-obsolete-funcs
               (when (>= emacs-major-version 25)
                 (list 'preceding-sexp)))))
  (require-package 'eval-sexp-fu))

(add-hook 'lisp-mode-hook #'(lambda ()
                              (eval-sexp-fu-flash-mode 1)))

(provide 'init-eval-sexp-fu)
