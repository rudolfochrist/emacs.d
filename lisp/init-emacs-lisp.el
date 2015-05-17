
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(defun fyi-lisp-after-save ()
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
    (check-parens)))
(add-hook 'after-save-hook #'fyi-lisp-after-save)

(provide 'init-emacs-lisp)
