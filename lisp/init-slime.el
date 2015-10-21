(require-package 'slime)
(require-package 'slime-company)

(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'inferior-lisp-mode-hook #'inferior-slime-mode)

(setq slime-complete-symbol*-fancy t
      slime-complete-symbol-function #'slime-fuzzy-complete-symbol
      slime-startup-animation t
      slime-net-coding-system 'utf-8-unix)

;;; Multiple Lisps
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))
        (acl ("/Applications/AllegroCLexpress.app/Contents/Resources/alisp"))
        (ccl ("/usr/local/bin/ccl64"))
        (ccl-trunk ("/usr/local/src/ccl/scripts/ccl64"))
        (clisp ("/usr/local/bin/clisp"))
        (ecl ("/usr/local/bin/ecl"))))

;;; getting contrib fancy
(slime-setup '(slime-fancy slime-repl slime-banner slime-mdot-fu slime-asdf slime-company))

;;; use slime-mode on asd files
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

;;; https://github.com/daimrod/Emacs-config/blob/master/config/config-slime.el
;; Add a directory to asdf:*central-registry*
(defslime-repl-shortcut slime-repl-add-to-central-registry
    ("add-to-central-registry" "+a" "add")
  (:handler (lambda (directory)
              (interactive
               (list (expand-file-name (file-name-as-directory
                                        (read-directory-name
                                         "Add directory: "
                                         (slime-eval '(swank:default-directory))
                                         nil nil "")))))
              (insert "(cl:pushnew (cl:truename #P\"" directory "\") asdf:*central-registry* :test #'equal)")
              (slime-repl-send-input t)))
  (:one-liner "Add a directory to asdf:*central-registry*"))

(defslime-repl-shortcut slime-repl-quicklisp-quickload
    ("quicklisp-quickload" "ql")
  (:handler (lambda (&rest systems)
              (interactive (list (slime-read-system-name)))
              (insert (format "(ql:quickload '%s)" systems))
              (slime-repl-send-input t)))
  (:one-liner "cl:quickload system"))

(defslime-repl-shortcut slime-repl-quickload-and-switch
    ("quickload-and-switch" "qs")
  (:handler (lambda (system)
              (interactive (list (slime-read-system-name)))
              (insert (format "(progn (in-package :cl-user) (ql:quickload :%s) (in-package :%s))" system system))
              (slime-repl-send-input t)))
  (:one-liner "quickload and switch to system"))

;;; Lookup documenation in Info hyperspec
;;; need either https://github.com/RobBlackwell/dpans2texi
;;; or the GNU Common Lips Info files [https://www.gnu.org/software/gcl/]
(defun fyi-hyperspec-info-lookup (symbol)
  (interactive (list (read-string "Lookup Hyperspec: " (thing-at-point 'symbol))))
  (condition-case nil
      (progn
        (info "(ansicl/ansicl)")
        (Info-index symbol))
    ('user-error
     (message "%s not found!" symbol)
     (Info-exit))))

(defun fyi-slime-autodoc-newline ()
  (interactive)
  (if (eq major-mode 'slime-repl-mode)
      (slime-repl-newline-and-indent)
      (newline-and-indent))
  (let ((doc (slime-autodoc t)))
    (when doc
      (eldoc-message "%s" doc))))
(eldoc-add-command 'fyi-slime-autodoc-newline)

(defun fyi-slime-keybindings ()
  (define-key slime-mode-map (kbd "C-c C-d h") #'fyi-hyperspec-info-lookup)
  (define-key slime-mode-map (kbd "C-c C-d C-h") #'fyi-hyperspec-info-lookup)
  (define-key slime-mode-map (kbd "RET") #'fyi-slime-autodoc-newline))

(add-hook 'slime-mode-hook 'fyi-slime-keybindings)

(defun fyi-slime-repl-keybindings ()
  (define-key slime-repl-mode-map (kbd "C-c C-d h") #'fyi-hyperspec-info-lookup)
  (define-key slime-repl-mode-map (kbd "C-d C-h") #'fyi-hyperspec-info-lookup)
  (define-key slime-repl-mode-map (kbd "C-l") #'slime-repl-clear-buffer)
  (define-key slime-repl-mode-map (kbd "SPC") #'slime-autodoc-space)
  (define-key slime-repl-mode-map (kbd "C-j") #'fyi-slime-autodoc-newline))

(add-hook 'slime-repl-mode-hook 'fyi-slime-repl-keybindings)

(provide 'init-slime)
