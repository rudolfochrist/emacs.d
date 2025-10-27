;;; dot-slime.el --- My Slime config

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
(require 'project)

;;; some initialization
(setq slime-complete-symbol*-fancy t
      slime-startup-animation t
      slime-net-coding-system 'utf-8-unix
      slime-documentation-lookup-function #'slime-eww-hyperspec-lookup)

(setq slime-lisp-implementations
      '((sbcl ("sbcl"))))

(setq slime-completion-at-point-functions
      '(slime-filename-completion
        slime-fuzzy-complete-symbol))

(setq slime-contribs
      '(slime-fancy
        slime-banner
        slime-asdf
        slime-tramp
        slime-xref-browser
        slime-highlight-edits
        slime-sprof
        ;; third partt
        slime-company))

(bind-key "C-. L" #'slime-connect)
(bind-key "C-. C-/" #'slime-selector)

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

(advice-add 'slime-set-default-directory :after
            (lambda (directory)
              (when (slime-eval '(cl:member :clm cl:*features*))
                (slime-eval `(cl:setf (clm:env) ,(slime-to-lisp-filename directory)))))
            '((name . "set-clm-env")))


;;; slime-selector

(def-slime-selector-method ?a "Visit system definition (asd) buffer."
  (local/find-project-asd))


;;; Slime TRACE Dialog
;;; For some reason the keys are not bound. Rebind them.
(bind-key "C-c M-t" #'slime-trace-dialog-toggle-trace slime-mode-map)
(bind-key "C-c T" #'slime-trace-dialog slime-mode-map)

;;; entry

(defun my-start-slime ()
  "Start slime."
  (interactive)
  (slime))

(provide 'dot-slime)

;;; dot-slime.el ends here
