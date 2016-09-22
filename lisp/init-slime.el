(require-package 'slime)
(require-package 'slime-company)

;;; HyperSpec/Documentation
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(require-package 'quicklisp-docs
                 :from-dir (expand-file-name "~/dev/quicklisp-docs/"))
(setq ql-docs-browser-function #'eww-browse-url)
(ql-docs-reload-docs)

(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'inferior-lisp-mode-hook #'inferior-slime-mode)

(setq slime-complete-symbol*-fancy t
      slime-complete-symbol-function #'slime-fuzzy-complete-symbol
      slime-startup-animation t
      slime-net-coding-system 'utf-8-unix)

;;; Multiple Lisps
(setq slime-lisp-implementations
      '((ccl ("/usr/local/bin/ccl64"))
        (sbcl ("/usr/local/bin/sbcl"))
        (ecl ("/usr/local/bin/ecl"))
        (clisp ("/usr/local/bin/clisp"))
        (abcl ("/usr/local/bin/abcl"))
        (acl ("/Applications/AllegroCLexpress.app/Contents/Resources/alisp"))))

;;; getting contrib fancy
(slime-setup '(slime-fancy
               slime-banner slime-asdf slime-company
               slime-tramp slime-xref-browser slime-highlight-edits
               slime-sprof slime-macrostep slime-indentation))

;;; set highlight-edits faces
(with-eval-after-load "slime-highlight-edits"
  (custom-set-faces
   '(slime-highlight-edits-face ((t (:slant italic))))))


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

;;; show autodoc also on newline.
(defun fyi-slime-autodoc-newline ()
  (interactive)
  (if (eq major-mode 'slime-repl-mode)
      (slime-repl-newline-and-indent)
    (newline-and-indent))
  (let ((doc (slime-autodoc t)))
    (when doc
      (eldoc-message "%s" doc))))
(eldoc-add-command 'fyi-slime-autodoc-newline)

;;; switch to repl and back again
(defun fyi-slime-repl-switch ()
  (interactive)
  (cond
   ((eq major-mode 'slime-repl-mode)
    (other-window 1)
    (switch-to-buffer (first (fyi-mode-buffer-list 'lisp-mode))))
   (t
    (slime-switch-to-output-buffer))))

(defun fyi-open-lisp ()
  (interactive)
  (let ((repl-buffers (fyi-mode-buffer-list 'slime-repl-mode)))
    (cond
     ;; no REPL active. Start new.
     ((null repl-buffers)
      (call-interactively #'slime))
     ;; one REPL active. Switch to it.
     ((= 1 (length repl-buffers))
      (switch-to-buffer (first repl-buffers)))
     ;; ivy select buffer
     (t
      (ivy-read "Lisp REPL: " (mapcar #'buffer-name repl-buffers)
                :require-match t
                :action (lambda (buffer)
                          (switch-to-buffer buffer)))))))
(global-set-key (kbd "<f5>") #'fyi-open-lisp)

;;; file header
(defun fyi-slime-insert-header (cl-style coding)
  (interactive
   (cond
    ((not current-prefix-arg)
     (list nil nil))
    ((equal current-prefix-arg '(4))
     (list "modern" "utf-8-unix"))
    ((equal current-prefix-arg '(16))
     (list
      (completing-read "Style: " common-lisp-styles)
      (completing-read "Coding: " coding-system-list)))))
  (when (and cl-style coding)
    (insert (format ";;; -*- mode: Lisp; common-lisp-style: %s; slime-coding: %s -*-\n;;;\n"
                    cl-style coding)))
  (let ((file-name (buffer-file-name)))
    (insert ";;; " (file-name-base file-name) (file-name-extension file-name t))))

;;; Inspect last expresssion
(defun slime-repl-inspect-last-expression ()
  "Inspects the last expression."
  (interactive)
  (slime-repl-inspect "*"))

;;; docstring in minibuffer
(defun slime-documentation-in-minibuffer (symbol)
  (interactive
   (let ((s-a-p (slime-symbol-at-point)))
     (cond
      ((or current-prefix-arg
           (not s-a-p))
       (list (slime-read-symbol-name "Symbol: ")))
      (t
       (list s-a-p)))))
  (message "%s"
           (slime-eval `(swank:documentation-symbol ,symbol))))

(defun fyi-slime-keybindings ()
  (define-key slime-mode-map (kbd "RET") #'fyi-slime-autodoc-newline)
  (define-key slime-mode-map (kbd "C-c C-z") #'fyi-slime-repl-switch)
  (define-key slime-mode-map (kbd "C-c C-d i") #'fyi-slime-insert-header)
  (define-key slime-mode-map (kbd "C-c C-d s") #'slime-documentation-in-minibuffer))

(add-hook 'slime-mode-hook 'fyi-slime-keybindings)

(defun fyi-slime-repl-keybindings ()
  (define-key slime-repl-mode-map (kbd "C-l") #'slime-repl-clear-buffer)
  (define-key slime-repl-mode-map (kbd "SPC") #'slime-autodoc-space)
  (define-key slime-repl-mode-map (kbd "C-j") #'fyi-slime-autodoc-newline)
  (define-key slime-repl-mode-map (kbd "C-c C-z") #'fyi-slime-repl-switch)
  (define-key slime-repl-mode-map (kbd "C-c O") #'slime-repl-inspect-last-expression)
  (define-key slime-repl-mode-map (kbd "C-c C-d s") #'slime-documentation-in-minibuffer))

(add-hook 'slime-repl-mode-hook 'fyi-slime-repl-keybindings)

(provide 'init-slime)
