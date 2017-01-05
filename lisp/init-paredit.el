(require-package 'paredit)
(require-package 'paredit-everywhere)
(require 'cl-lib)

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

(defvar paredit-custom-keys-alist
  '(
    ;; better parens
    ("[" . paredit-open-round)
    ("]" . paredit-close-round)
    ("(" . paredit-open-bracket)
    (")" . paredit-close-bracket)
    ;; navigation
    ("s-h" . paredit-backward)
    ("s-l" . paredit-forward)
    ("s-k" . paredit-backward-up)
    ("s-j" . paredit-forward-down)
    ("s-<left>" . paredit-backward)
    ("s-<right>" . paredit-forward)
    ("s-<up>" . paredit-backward-up)
    ("s-<down>" . paredit-forward-down)
    ;; misc.
    ("C-c C-o" . paredit-insert-section-header)))

(defun paredit-custom-keybindings ()
  (cl-loop for (key . func) in paredit-custom-keys-alist
           do (define-key paredit-mode-map (kbd key) func)))      
(add-hook 'paredit-mode-hook #'paredit-custom-keybindings)

;;; use `common-lisp-indent-function'
(setq lisp-indent-function #'common-lisp-indent-function)
(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-function
                        #'common-lisp-indent-function)))

(defun paredit-insert-section-header (header)
  "Insert a banner with format ;;; HEADER ;;;."
  (interactive "sDescription: ")
  (let* ((header-length 80)
         (trans-header (format "  %s  " (upcase header)))
         (banner (s-join "" (make-list (floor (- header-length (length trans-header)) 2) ";"))))
    (insert (kbd "C-l") "\n")
    (insert (concat banner trans-header banner))))

(provide 'init-paredit)
