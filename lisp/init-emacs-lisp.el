
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(defun fyi-lisp-after-save ()
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
    (check-parens)))
(add-hook 'after-save-hook #'fyi-lisp-after-save)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-function #'lisp-indent-function)))

(provide 'init-emacs-lisp)
