(require-package 'paredit)
(require-package 'paredit-everywhere)

(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook #'enable-paredit-mode)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;;; don't add space before splicing (,@) or reader macros
(defun fyi-paredit-adjust-spacing (endp delimiter)
  (cond
   ((or (looking-back "#\\+.*")
        (looking-back "#-.*"))
    t)
   ((looking-back "#.*")
    nil)
   ((looking-back ",@")
    nil)
   (t t)))

(add-to-list 'paredit-space-for-delimiter-predicates 'fyi-paredit-adjust-spacing)

;;; make parentheses unshifted in lisp mode
(unless (null paredit-mode-map)
  (define-key paredit-mode-map (kbd "[") 'paredit-open-round)
  (define-key paredit-mode-map (kbd "]") 'paredit-close-round)
  (define-key paredit-mode-map (kbd "(") 'paredit-open-bracket)
  (define-key paredit-mode-map (kbd ")") 'paredit-close-bracket)
  (define-key paredit-mode-map (kbd "C-c C-o") 'paredit-insert-section-header))

;;; use `common-lisp-indent-function'
(setq lisp-indent-function #'common-lisp-indent-function)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-function
                        #'common-lisp-indent-function)))

;;; hippie-expand: remove last )
;;; http://www.emacswiki.org/emacs/HippieExpand#toc7
(defun fyi-he-substitute-string (str &optional ops)
  "remove extra paren when expanding line in paredit"
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))
(advice-add 'he-substitute-string :after 'fyi-he-substitute-string)

(require 's)

(defun paredit-insert-section-header (header)
  "Insert a banner with format ;;; HEADER ;;;."
  (interactive "sDescription: ")
  (let* ((header-length 80)
         (trans-header (format "  %s  " (upcase header)))
         (banner (s-join "" (make-list (floor (- header-length (length trans-header)) 2) ";"))))
    (insert (kbd "C-l") "\n")
    (insert (concat banner trans-header banner))))

(provide 'init-paredit)
