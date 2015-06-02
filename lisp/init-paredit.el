(require-package 'paredit)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)

;;; make parentheses unshifted in lisp mode
(unless (null paredit-mode-map)
  (define-key paredit-mode-map (kbd "[") 'paredit-open-round)
  (define-key paredit-mode-map (kbd "]") 'paredit-close-round)
  (define-key paredit-mode-map (kbd "(") 'paredit-open-bracket)
  (define-key paredit-mode-map (kbd ")") 'paredit-close-bracket)

  (define-key paredit-mode-map
      (kbd "qq")
    (defhydra hydra-paredit (:hint nil
                             :idle )
      "
^Movement & Navigation^
-----------------------
_h_: backward sexp   _H_: previous line
_j_: forward down    _J_: backward down    _a_: beginning of sexp
_k_: backward up     _K_: forward up       _e_: end of sexp
_l_: forward sexp    _L_: next line
"
      ("h" paredit-backward)
      ("j" paredit-forward-down)
      ("k" paredit-backward-up)
      ("l" paredit-forward)
      ("H" previous-line)
      ("J" paredit-backward-down)
      ("K" paredit-forward-up)
      ("L" next-line)
      ("a" (lambda ()
             (interactive)
             (paredit-backward-up)
             (forward-char)))
      ("e" (lambda ()
             (interactive)
             (paredit-forward-up)
             (backward-char))))))

;;; use `common-lisp-indent-function'
(setq lisp-indent-function #'common-lisp-indent-function)

;;; hippie-expand: remove last )
;;; http://www.emacswiki.org/emacs/HippieExpand#toc7
(defun fyi-he-substitute-string (str &optional ops)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))
(advice-add 'he-substitute-string :after 'fyi-he-substitute-string)




(provide 'init-paredit)
