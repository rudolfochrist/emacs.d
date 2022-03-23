;;; dot-slime.el --- My Slime config
;; company backend

;;; Commentary:
;; Use at your own risk

;;; Code:

(use-package slime :ensure t)
(use-package slime-company
  :ensure t
  :no-require t
  :after (slime company)
  :config
  (setq slime-company-completion 'fuzzy))
(require 'slime-repl)
(require 'eldoc)
(require 'find-file-in-project)
(require 'ivy)
(require 'font-lock)

;;; some initialization
(setq slime-complete-symbol*-fancy t
      slime-startup-animation t
      slime-net-coding-system 'utf-8-unix
      slime-documentation-lookup-function #'slime-eww-hyperspec-lookup)

(setq slime-lisp-implementations
      '((sbcl ("sbcl"))
        (alisp ("alisp"))
        (ccl ("ccl64"))))

(setq slime-completion-at-point-functions
      '(slime-filename-completion
        slime-fuzzy-complete-symbol))

(setq slime-contribs
      '(slime-fancy
        slime-trace-dialog
        slime-banner
        slime-asdf
        slime-company
        slime-tramp
        slime-xref-browser
        slime-highlight-edits
        slime-sprof
        slime-indentation))

(bind-key "C-. L" #'slime-connect)
(bind-key "C-. C-/" #'slime-selector)

(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'inferior-lisp-mode-hook #'inferior-slime-mode)

(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.cl\\'" .lisp-mode))

(add-hook 'lisp-mode-hook
          (lambda ()
            (setq-local lisp-indent-function #'common-lisp-indent-function)))

(load "indentation-rules.el" t)

;;; some convenient functions

(defun slime-autodoc-newline ()
  "Provide autodoc also on newline."
  (interactive)
  (if (eq major-mode 'slime-repl-mode)
      (slime-repl-newline-and-indent)
    (newline-and-indent))
  (let ((doc (slime-autodoc t)))
    (when doc
      (eldoc-message (format "%s" doc)))))

(eldoc-add-command 'slime-autodoc-newline)


(defun slime-repl-inspect-last-expression ()
  "Inspects the last expression."
  (interactive)
  (slime-repl-inspect "*"))


;;; show hyperspec in EWW
(defun slime-eww-hyperspec-lookup ()
  "Open the hyperspec in EWW inside Emacs."
  (interactive)
  (let ((browse-url-browser-function #'eww-browse-url))
    (call-interactively #'slime-hyperspec-lookup)))

(defun my-common-lisp-hyperspec-format ()
  "Open format directives in EWW."
  (interactive)
  (let ((browse-url-browser-function #'eww-browse-url))
    (call-interactively #'common-lisp-hyperspec-format)))
(bind-key "C-c C-d ~" #'my-common-lisp-hyperspec-format slime-doc-map)

(defun my-common-lisp-hyperspec-glossary-term ()
  "Open glossary in EWW."
  (let ((browse-url-browser-function #'eww-browse-url))
    (call-interactively #'common-lisp-hyperspec-glossary-term)))
(bind-key "C-c C-d g" #'my-common-lisp-hyperspec-glossary-term slime-doc-map)

(defun my-common-lisp-hyperspec-lookup-reader-macro ()
  "Open reader macros in EWW."
  (interactive)
  (let ((browse-url-browser-function #'eww-browse-url))
    (call-interactively #'common-lisp-hyperspec-lookup-reader-macro)))
(bind-key "C-c C-d #" #'my-common-lisp-hyperspec-lookup-reader-macro slime-doc-map)


(bind-key "RET" #'slime-autodoc-newline slime-mode-map)

;; HyperSpec/Documentation
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

;;; REPL setup
(bind-key "C-l" #'slime-repl-clear-buffer slime-repl-mode-map)
(bind-key "SPC" #'slime-autodoc-space slime-repl-mode-map)
(bind-key "C-j" #'slime-autodoc-newline slime-repl-mode-map)
(bind-key "C-c O" #'slime-repl-inspect-last-expression slime-repl-mode-map)

(defslime-repl-shortcut slime-repl-quicklisp-quickload
  ("quicklisp-quickload" "ql")
  (:handler (lambda (&rest systems)
              (interactive (list (slime-read-system-name)))
              (insert (format "(ql:quickload '%s)" systems))
              (slime-repl-send-input t)))
  (:one-liner "cl:quickload system"))


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


(defslime-repl-shortcut slime-dired-system-source-directory
  ("dired-system-source-directory" "dired-system" "ds")
  (:handler (lambda (system)
              (interactive (list (slime-read-system-name)))
              (let ((path (slime-eval `(cl:namestring (asdf:system-source-directory ,system)))))
                (dired path)))))


;;; slime-selector

(defun slime-find-project-asd ()
  "Find the project ASD file."
  (let ((asds (directory-files (ffip-project-root) t ".asd")))
    (cond
     ((zerop (length asds))
      (user-error "No ASD fiiles found!"))
     ((= (length asds) 1)
      (find-file (car asds)))
     (t
      (find-file (ivy-completing-read "ASD file: " asds nil t))))))

(def-slime-selector-method ?a "Visit system definition (asd) buffer."
  (slime-find-project-asd))

;;; xref ivy completion

(defun slime-package-symbols ()
  (slime-eval `(swank-user:package-symbols ,(slime-current-package))))

(defun slime-read-from-minibuffer (prompt &optional initial-value history)
  "Like the original but with `ivy'."
  (let ((minibuffer-setup-hook (slime-minibuffer-setup-hook)))
    (ivy-completing-read prompt
                         (slime-package-symbols)
                         nil
                         t
                         initial-value
                         (or history slime-minibuffer-history))))

;;; Slime TRACE Dialog
;;; For some reason the keys are not bound. Rebind them.
(bind-key "C-c M-t" #'slime-trace-dialog-toggle-trace slime-mode-map)
(bind-key "C-c T" #'slime-trace-dialog slime-mode-map)

(defun my-start-slime ()
  "Start slime."
  (interactive)
  (slime))

(provide 'dot-slime)

;;; dot-slime.el ends here
