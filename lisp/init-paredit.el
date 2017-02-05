(require-package 'paredit-everywhere)

;;; add to slime
;;; use `common-lisp-indent-function'
(setq lisp-indent-function #'common-lisp-indent-function)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-function
                        #'common-lisp-indent-function)))

