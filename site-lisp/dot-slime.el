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


(defslime-repl-shortcut slime-repl-project-loader-load-system
  ("pl-load-system" "pl")
  (:handler (lambda (system)
              (interactive (list (slime-read-system-name)))
              (insert (format "(pl:load-system \"%s\")" system))
              (slime-repl-send-input t)))
  (:one-liner "pl:load-system system"))


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

(advice-add 'slime-set-default-directory :after
            (lambda (directory)
              (when (slime-eval '(cl:member :clm cl:*features*))
                (slime-eval `(cl:setf (clm:env) ,(slime-to-lisp-filename directory)))))
            '((name . "set-clm-env")))


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


;;; Slime TRACE Dialog
;;; For some reason the keys are not bound. Rebind them.
(bind-key "C-c M-t" #'slime-trace-dialog-toggle-trace slime-mode-map)
(bind-key "C-c T" #'slime-trace-dialog slime-mode-map)

;;; test runner
;;; inspired by:
;;; https://salsa.debian.org/intrigeri/lunar-mode-line.el/-/blob/master/lunar-mode-line.el

(defvar st-mode-directory (file-name-directory (or load-file-name buffer-file-name)))
(defvar st-pass-image (concat st-mode-directory "images/st-pass.png"))
(defvar st-fail-image (concat st-mode-directory "images/st-fail.png"))
(defvar st-mode-line " st")

(defun st-make-mode-line (file)
  (propertize " st"
              'display (create-image file 'png nil
                                     :ascent 'center
                                     :background (face-background 'mode-line))
              'help-echo "St Mode Status"))

(defun st-pass ()
  (setq st-mode-line (st-make-mode-line st-pass-image))
  (force-mode-line-update))

(defun st-fail ()
  (setq st-mode-line (st-make-mode-line st-fail-image))
  (force-mode-line-update))

(defun st-find-buffer-suite ()
  (let ((regexp "(\\s-*\\(\\(it.bese.\\)?fiveam\\|5am\\)?:?def-suite\\*?\\s-+\\([^)]+\\)\\s-*)"))
    (save-restriction
      (widen)
      (save-excursion
        (when (or (re-search-backward regexp nil t)
                  (re-search-forward regexp nil t))
          (car (split-string (match-string-no-properties 3))))))))


(defun st-run-tests ()
  (interactive)
  (let ((suite (st-find-buffer-suite)))
    (slime-eval-async `(fiveam:run! ,(slime-keywordify (make-symbol suite)))
      (lambda (pass)
        (if pass
            (st-pass)
          (st-fail))))))

(bind-key "C-c C-d t" #'st-run-tests slime-mode-map)

(defun st-mode-line-string ()
  st-mode-line)

(define-minor-mode st-mode
  "Show test status in mode-line."
  :lighter (:eval (st-mode-line-string)))

(add-hook 'slime-mode-hook #'st-mode)

;;; enable in all slime-mode buffers
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when slime-mode
      (st-mode 1))))

;;; common lisp info files quick-access

(defun open-system-info (files)
  (interactive
   (list (let ((system (slime-read-system-name)))
           (if (slime-eval `(cl:and (asdf:find-system ,system nil) t))
               (slime-eval `(cl:mapcar #'cl:namestring
                                       (uiop:directory-files
                                        (asdf:system-source-directory ,system)
                                        "**/*.info")))
             (user-error "System %s not found" system)))))
  (cl-case (length files)
    (0 (message "No info files found!"))
    (1 (info (car files)))
    (t (info (ivy-completing-read "Select file: " files nil t)))))

(bind-key "C-c C-d i" #'open-system-info slime-mode-map)
(bind-key "C-c C-d i" #'open-system-info slime-repl-mode-map)

;;; entry

(defun my-start-slime ()
  "Start slime."
  (interactive)
  (slime))

(provide 'dot-slime)

;;; dot-slime.el ends here
