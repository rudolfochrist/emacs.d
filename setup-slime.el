(load (expand-file-name "~/quicklisp/slime-helper.el"))

(add-hook 'lisp-mode-hook #'slime-mode)
(setq slime-complete-symbol-function #'slime-fuzzy-complete-symbol)

;;; use UTF-8
(setq slime-net-coding-system 'utf-8-unix)

;;; Multiple Lisps
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))
        (ccl ("/usr/local/bin/ccl"))
        (abcl ("/usr/local/bin/abcl") :env ("PATH=/usr/local/bin:/usr/bin:$PATH"))))

;;; getting contrib fancy
(setq slime-contribs '(slime-fancy))


(provide 'setup-slime)
