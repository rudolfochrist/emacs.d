(load (expand-file-name "~/quicklisp/slime-helper.el"))


;;; setup local hyperspec
;;; see (ql:quickload "clhs") for more information
(load (expand-file-name "~/quicklisp/clhs-use-local.el"))

(add-hook 'lisp-mode-hook #'slime-mode)
(setq slime-complete-symbol*-fancy t)
(setq slime-complete-symbol-function #'slime-fuzzy-complete-symbol)

;;; use UTF-8
(setq slime-net-coding-system 'utf-8-unix)

;;; Multiple Lisps
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))
        (ccl ("/usr/local/bin/ccl"))1
        (allegro ("/Applications/AllegroCLexpress.app/Contents/Resources/alisp"))
        (abcl ("/usr/local/bin/abcl") :env ("PATH=/usr/local/bin:/usr/bin:$PATH"))))

;;; getting contrib fancy
(setq slime-contribs '(slime-fancy slime-banner))

;;; but disable autodoc
(setq slime-use-autodoc-mode nil)

;;; use slime-mode on asd files
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))


(provide 'setup-slime)
