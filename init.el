;;; init.el --- My Emacs Initialization File
;;; My emacs config.

;;; Enable package management

;;; Commentary:
;; Nothing special here.

;;; Code:
(require 'package)
(setf package-enable-at-startup nil
      package-user-dir "~/.emacs-packages")
(if (not (gnutls-available-p))
    (warn "Packages cannot use HTTPS. Please install gnutls")
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			   ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

;; use CL features
(require 'cl-lib)

;;; load-path
(defvar site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory))
(add-to-list 'load-path site-lisp-directory)

;;; prefer newer files
(setq load-prefer-newer t)

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; setup use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(defvar use-package-enable-imenu-support t)
(require 'bind-key)
(require 'use-package)


;;; BASICS

(setq inhibit-startup-message t
      initial-scratch-message nil
      system-uses-terminfo nil         ; use Emacs terminfo
      backup-inhibited t               ; disable backups
      auto-save-default nil            ; Nice feature, but annoying as well. Disable it.
      ring-bell-function 'ignore       ; don't ring the bell
      scroll-preserve-screen-position 'always
      confirm-kill-emacs 'yes-or-no-p
      scroll-margin 2
      mac-command-modifier 'control    ; so much nicer on MacOS
      mac-control-modifier 'command)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; set up environment details.
;;; especially Perl is bitchy about it if you run commands from within
;;; Emacs

;;; setup paths
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (not (eq system-type 'windows-nt))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "TERM" "CC")))

;;; disable GUI stuff
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; frame setup
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(height . 55))

;;; tabs, spaces, indentation, parens
(require 'paren)
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      require-final-newline t
      show-paren-style 'mixed)
(show-paren-mode 1)

;;; mode-line
(require 'time)
(setq display-time-24hr-format t
      display-time-default-load-average nil
      display-time-day-and-date t
      display-battery-mode t)
(display-time-mode 1)
(line-number-mode 1)
(column-number-mode 1)

;; ;;; reset font colors in termins
;; (defun setup-terminal-colors (frame)
;;   (unless (display-graphic-p frame)
;;     (set-face-foreground 'default "white" frame)
;;     (set-face-background 'default "black" frame)))

;; (add-hook 'after-make-frame-functions #'setup-terminal-colors)

;; ;;; default theme with custom background. Only in GUI Emacs.
;; ;;; see http://irreal.org/blog/?p=3900
;; (when (display-graphic-p)
;;   (set-background-color "white smoke")
;;   (add-to-list 'default-frame-alist '(background-color . "white smoke")))


;;; make scripts executable if shebang present
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; macOS lacks a compose key, so we're setting the input-method
(when (eql 'darwin system-type)
  (add-hook 'text-mode-hook
            (lambda ()
              (set-input-method "latin-9-prefix"))))


;;; SERVER

(require 'server)
(when (and (not (getenv "EMACS_NO_SERVER"))
           (not (server-running-p)))
  (server-start))



;;; UTILITIES

(defun system-macos-p ()
  "Return non-nil if `system-type' is 'darwin."
  (eql 'darwin system-type))

(defun mode-buffer-list (mode)
  "Return a list of buffers with major mode MODE."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (eql major-mode mode)))
   (buffer-list)))

(defun lookup-binding (command)
  "Return the keybinding for COMMAND."
  (substitute-command-keys (format "\\[%s]" command)))

(defun ansi-colorize-buffer ()
  "Replace ANSI color markers with colored text."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))




;;; KEYBINDINGS
;;;
;;; M-s is for search
;;; M-g is for goto
;;; C-. prefix map is for personal bindings

;;; C-.
;;; unbind in flyspell-mode
(with-eval-after-load 'flyspell
  (unbind-key "C-." flyspell-mode-map))

(defvar ctl-period-map nil)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." #'ctl-period-map)

;;; killing the whole region
(bind-key "C-x C-k" #'kill-region)

;;; buffer switching
(bind-key "C-c C-b" #'ibuffer)

;;; fullscreen
(bind-key "C-. y" #'toggle-frame-fullscreen)

;;; compiling
(bind-key "C-. b" #'compile)

;;; killing a buffer actually should bury it.
(bind-key "C-x k" #'bury-buffer)
;;; except you mean it
(bind-key "C-x K" #'kill-buffer)

;;; reverting
(defun revert-buffer-no-confirm ()
  "Revert current buffer but don't nag."
  (interactive)
  (revert-buffer t t))
(bind-key "C-c C-r" #'revert-buffer-no-confirm)

;;; libs

(use-package f        :defer t :ensure t)
(use-package s        :defer t :ensure t)
(use-package dash     :defer t :ensure t)

;;; swiper, ivy, counsel
;;; https://github.com/abo-abo/swiper

(use-package smex
  :ensure t
  :defer t)

(use-package ivy
  :ensure t
  :demand t
  :bind (("C-. C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer))
  :init
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-. C-," . counsel-imenu)
         ("C-. B" . counsel-bookmark)))

;;; info

(setq Info-directory-list
      (append (list (expand-file-name "~/info"))
              Info-default-directory-list))

(use-package info
  :commands (info)
  :bind (("C-h a" . apropos)
         ("C-h A" . apropos))
  :demand t
  :config
  (use-package info-look
    :bind (("C-h S" . info-lookup-symbol))))


;;; flyspell

(use-package flyspell
  :unless (eq window-system 'w32)
  :commands (flyspell-prog-mode flyspell-mode)
  :init
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;; flycheck

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;;; org-mode

(use-package dot-org
  :disabled t
  :load-path ("site-lisp/org-mode/lisp" "site-lisp/org-mode/contrib/lisp")
  :defer 10
  :bind (("C-c l" . org-store-link)
         ("C-. a" . org-agenda)
         ("C-. c" . org-smart-capture)
         :map org-mode-map
         ("C-c C-r" . org-refile-web-capture))
  :init
  (add-to-list 'Info-directory-list
               (expand-file-name "org-mode/doc" site-lisp-directory)))


;;; wgrep

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-change)
  :init (setq wgrep-auto-save-buffer t))


;;; company-mode

(use-package company
  :load-path "site-lisp/company-mode"
  :commands (global-company-mode)
  :bind (("C-. C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :hook (after-init . global-company-mode))


;;; dired

(use-package dired
  :bind (("C-x D" . dired)
         :map
         dired-mode-map
         ("M-!" . async-shell-command)
         ("l" . dired-up-alternate-directory)
         ("RET" . dired-find-alternate-file)
         ("M-RET" . dired-open-natively))
  :preface
  (defun dired-open-natively ()
    "Opens file with the native app."
    (interactive)
    (let ((cmd (if (eql system-type 'darwin)
                   "open"
                 "xdg-open")))
      (shell-command (format "%s %s"
                             cmd
                             (shell-quote-argument (dired-get-file-for-visit))))))
  (defun dired-up-alternate-directory ()
    (interactive)
    (find-alternate-file ".."))
  :init
  (setq dired-garbage-files-regexp
        "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'"
        dired-listing-switches "-alvh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-dwim-target t)
  (when (system-macos-p)
    (setq insert-directory-program "/usr/local/bin/gls")))

(use-package dired-x
  :after dired
  :bind (("C-x d" . dired-jump)))

(use-package dired-narrow
  :ensure t
  :after dired
  :commands (dired-narrow)
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package find-dired
  :bind (("C-. w" . find-name-dired)))


;;; ediff

(use-package ediff
  :init
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ;; ignores certain changes
        ediff-diff-options "-w"))


;;; iedit

(use-package iedit
  :ensure t
  :bind (("C-. C-'" . iedit-mode)
         ("C-. C-\"" . iedit-mode-defun))
  :preface
  (defun iedit-mode-defun ()
    (interactive)
    (iedit-mode '(0))))


;;; js2-mode

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :init
  (setq js2-basic-offset 2
        js-indent-level 2))


;;; macrostep

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c M-e" . macrostep-expand)))


;;; magit

(use-package magit
  :ensure t
  :commands (magit-clone magit-blame)
  :bind (("C-. gg" . magit-status)
         ("C-. GG" . magit-status-with-prefix)
         ("C-. gl" . magit-list-repositories))
  :preface
  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/code/" . 1)
                                       ("~/prj/" . 1)
                                       ("~/quicklisp/local-projects/" . 1)
                                       ;; eclipse (necessary sometimes)
                                       ("~/workspace/" . 1)))
  ;; install info
  (add-to-list 'Info-directory-list
               (expand-file-name "magit/Documentation" site-lisp-directory))
  :config
  (add-to-list 'magit-repolist-columns '("Dirty" 6 magit-repolist-column-dirty)))


;;; paredit

(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
  :bind (:map paredit-mode-map
              ("[" . paredit-open-round)
              ("]" . paredit-close-round)
              ("(" . paredit-open-bracket)
              (")" . paredit-close-bracket))
  :preface
  (defvar paredit-non-space-patterns '("#\+" "#-" "#." ",@"))

  (defun paredit-adjust-spacing-p (endp delimiter)
    "Don't add space before splicing (,@) or reader macros."
    (cl-notany #'looking-back paredit-non-space-patterns))
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (slime-repl-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode))
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates #'paredit-adjust-spacing-p)

  ;; using this for extended search
  (unbind-key "M-s" paredit-mode-map))

;;; which-key

(use-package which-key
  :ensure t
  :config (which-key-mode))


;;; whitespace

(use-package whitespace
  :init
  (setq whitespace-line-column 120
        whitespace-style '(face lines-tail tabs trailing))
  :hook (prog-mode . whitespace-mode))


;;; yasnippet

(use-package yasnippet
  :ensure t
  :commands (yas-expand yas-minor-mode)
  :hook ((text-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :config
  (yas-reload-all))


;;; emacs-lisp-mode

(use-package emacs-lisp-mode
  :mode (("Cask\\'" . emacs-lisp-mode))
  :init
  ;; emacs-lisp indentation
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local lisp-indent-function #'lisp-indent-function))))


;;; eldoc

(use-package eldoc
  :commands (eldoc-mode)
  :hook (emacs-lisp-mode . eldoc-mode)
  :config
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;;; slime

(use-package slime
  :ensure t
  :mode (("\\.asd\\'" . lisp-mode)
         ("\\.cl\\'" . lisp-mode))
  :commands (slime-mode slime-repl-mode)
  :bind (("C-. l" . slime)
         ("C-. L" . slime-connect)
         ("C-. C-/" . slime-selector)
         :map slime-mode-map
         ("RET" . slime-autodoc-newline)
         ("C-c C-d i" . slime-insert-file-header)
         :map slime-repl-mode-map
         ("C-l" . slime-repl-clear-buffer)
         ("SPC" . slime-autodoc-space)
         ("C-j" . slime-autodoc-newline)
         ("C-c O" . slime-repl-inspect-last-expression))
  :preface
  ;; show autodoc also on newline.
  (defun slime-autodoc-newline ()
    (interactive)
    (if (eq major-mode 'slime-repl-mode)
        (slime-repl-newline-and-indent)
      (newline-and-indent))
    (let ((doc (slime-autodoc t)))
      (when doc
        (eldoc-message "%s" doc))))
  (eldoc-add-command 'slime-autodoc-newline)

  ;; Inspect last expresssion
  (defun slime-repl-inspect-last-expression ()
    "Inspects the last expression."
    (interactive)
    (slime-repl-inspect "*"))

  (defun slime-insert-file-header ()
    "Inserts the current file name into the buffer.

Later renditions should locate the asd file and insert the whole
subpath."
    (interactive)
    (let ((file-name (file-name-nondirectory (buffer-file-name))))
      (goto-char (point-min))
      (insert
       (format ";;;; %s\n\n" file-name))))

  ;; custom slime-documentation-function
  (defun slime-eww-hyperspec-lookup ()
    "Opens the hyperspec in EWW inside Emacs."
    (interactive)
    (let ((browse-url-browser-function #'eww-browse-url))
      (call-interactively #'slime-hyperspec-lookup)))
  :hook ((lisp-mode . slime-mode)
         (inferior-lisp-mode . inferior-slime-mode))
  :init
  (setq slime-complete-symbol*-fancy t
        slime-complete-symbol-function #'slime-fuzzy-complete-symbol
        slime-startup-animation t
        slime-net-coding-system 'utf-8-unix
        slime-documentation-lookup-function #'slime-eww-hyperspec-lookup)

  ;; Lisps
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (ccl ("ccl64"))
          (alisp ("alisp"))))

  (setq slime-contribs
        '(slime-fancy
          slime-banner
          slime-asdf
          slime-company
          slime-tramp
          slime-xref-browser
          slime-highlight-edits
          slime-sprof
          slime-indentation))

  (add-hook 'lisp-mode-hook
            (lambda ()
              (setq-local lisp-indent-function #'common-lisp-indent-function)))
  ;; slime docs
  (add-to-list 'Info-directory-list
               (expand-file-name "slime/doc" site-lisp-directory))
  :config
  ;; HyperSpec/Documentation
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

  ;; company backend
  (use-package slime-company
    :ensure t
    :demand t
    :init (setq slime-company-completion 'fuzzy)
    :commands (slime-company))

  ;; quicklisp REPL command
  (defslime-repl-shortcut slime-repl-quicklisp-quickload
    ("quicklisp-quickload" "ql")
    (:handler (lambda (&rest systems)
                (interactive (list (slime-read-system-name)))
                (insert (format "(ql:quickload '%s)" systems))
                (slime-repl-send-input t)))
    (:one-liner "cl:quickload system"))

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

  ;; open system directory in dired
  (defslime-repl-shortcut slime-dired-system-source-directory
    ("dired-system-source-directory" "dired-system" "ds")
    (:handler (lambda (system)
                (interactive (list (slime-read-system-name)))
                (let ((path (slime-eval `(cl:namestring (asdf:system-source-directory ,system)))))
                  (dired path)))))

  (defslime-repl-shortcut slime-load-local-system
    ("load-local-system" "load-local")
    (:handler (lambda (asd system-name)
                (interactive
                 (list
                  (read-file-name "ASD File: " nil nil t)
                  (read-minibuffer "System Name: ")))
                (insert (format "(progn (asdf:load-asd \"%s\") (asdf:load-system \"%s\"))"
                                asd
                                system-name))
                (slime-repl-send-input t)))))


;;; imenu

(defun lisp-imenu-defmethod-matcher ()
  "Match a Lisp defmethod and set `match-data' accordingly."
  (when (re-search-backward "defmethod" nil t)
    (ignore-errors
      (let ((data '()))
        (save-excursion
          ;; skip defmethod
          (forward-sexp)
          (forward-char)
          ;; beginning of index name
          (push (point-marker) data)
          ;; forward method name and lambda list
          (forward-sexp 2)
          ;; end of index name
          (push (point-marker) data))
        (set-match-data (append (match-data) (nreverse data)))
        ;; indicate success
        t))))

(use-package imenu
  :init
  ;; don't replace space
  (setq imenu-space-replacement nil)
  ;; overwrite how imenu handles defmethods. I want to see the
  ;; specializers for defmethod
  (with-eval-after-load 'lisp-mode
    (setq lisp-imenu-generic-expression
          (append
           ;; first remove current defmethod match expression
           (cl-remove nil lisp-imenu-generic-expression :key #'car)
           ;; add defmethod-specializer expression
           (list
            (list nil
                  #'lisp-imenu-defmethod-matcher
                  1)
            (list nil
                  (purecopy (concat "^\\s-*("
                                    (eval-when-compile
                                      (regexp-opt
                                       '("defun" "defmacro"
                                         ;; Elisp.
                                         "defun*" "defsubst" "define-inline"
                                         "define-advice" "defadvice" "define-skeleton"
                                         "define-compilation-mode" "define-minor-mode"
                                         "define-global-minor-mode"
                                         "define-globalized-minor-mode"
                                         "define-derived-mode" "define-generic-mode"
                                         "ert-deftest"
                                         "cl-defun" "cl-defsubst" "cl-defmacro"
                                         "cl-define-compiler-macro" "cl-defgeneric"
                                         "cl-defmethod"
                                         ;; CL.
                                         "define-compiler-macro" "define-modify-macro"
                                         "defsetf" "define-setf-expander"
                                         "define-method-combination"
                                         ;; CLOS and EIEIO
                                         ;; Look Ma! No defmethod here!
                                         "defgeneric")
                                       t))
                                    "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                  2))))))


;;; gnus

(use-package dot-gnus
  :disabled t
  :bind (("C-. m" . gnus)
         ("C-. M" . gnus-other-frame)))


;;; cperl-mode

(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :mode "cpanfile"
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode))
  :preface
  ;; https://www.emacswiki.org/emacs/CPerlMode#toc10
  (defun my-cperl-eldoc-documentation-function ()
    "Return meaningful doc string for `eldoc-mode'."
    (car
     (let ((cperl-message-on-help-error nil))
       (cperl-get-help))))
  :init
  (setq cperl-hairy t
        cperl-clobber-lisp-bindings nil
        cperl-info-on-command-no-prompt nil)
  (add-hook 'cperl-mode-hook
            (lambda ()
              (setq eldoc-documentation-function #'my-cperl-eldoc-documentation-function))
            nil
            t))


;;; Docker

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'"
  :commands (dockerfile-mode))


(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose.yml"
  :commands (docker-compose-mode))


;;; rich-minority

(use-package rich-minority
  :ensure t
  :demand t
  :init
  (setq rm-whitelist "\\[.*\\]")        ; just showing slime-mode: [package lisp-impl]
  :config
  (rich-minority-mode 1))


;;; sgml-mode

(use-package sgml-mode
  :commands (sgml-mode sgml-pretty-print))


;;; commit-msg-prefix

(use-package git-msg-prefix
  :ensure t
  :after magit
  :bind (:map
         ;; magit
         with-editor-mode-map
         ("C-. C-;" . git-msg-prefix))
  :init
  (setq git-msg-prefix-input-method #'ivy-read))


;;; helpful

(use-package helpful
  :ensure t
  :bind ( ("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)
          ("C-h ." . helpful-at-point)))

;;; web-mode

(use-package web-mode
  :ensure t
  :mode "\\.lsp\\'"
  :mode "\\.ep\\'"
  :preface
  (defun web-mode-lsp-bindings ()
    (bind-key "M-." #'slime-edit-definition web-mode-map)
    (bind-key "M-," #'slime-pop-find-definition-stack web-mode-map))
  :hook (web-mode . web-mode-lsp-bindings))


;;; zel

(use-package zel
  :ensure t
  :demand t
  :bind (("C-x C-r" . zel-find-file-frecent))
  :config
  (zel-install))


;;; JSON

(use-package json-reformat
  :ensure t)

(use-package json-snatcher
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")


;;; log file reading
;;; https://writequit.org/articles/working-with-logs-in-emacs.html

(use-package vlf
  :ensure t)

(use-package logview
  :ensure t
  :mode ("\\.log\\'" . logview-mode)
  :hook (logview-mode . hl-line-mode)
  :config
  ;; wildfly submode
  (add-to-list 'logview-additional-level-mappings
               '("WILDFLY"
                 (error "ERROR")
                 (warning "WARN" "WARNING")
                 (information "INFO")
                 (debug "DEBUG" "FINE")
                 (trace "TRACE" "FINER")))
  (add-to-list 'logview-additional-submodes
               '("Wildfly"
                 (format . "TIMESTAMP LEVEL [NAME] (THREAD)")
                 (levels . "WILDFLY"))))


;;; tide (TypeScript)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'")

(use-package tide
  :ensure t
  :after (typescript-mode)
  :commands (tide-setup tide-hl-identifier-mode tide-format-before-save)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :bind (:map tide-mode-map
              ("C-c r" . tide-references)
              ("C-c ." . tide-fix)
              ("C-c o" . tide-organize-imports)
              ("C-c p" . tide-project-errors)))

;;; abbrev

(use-package abbrev
  :hook ((text-mode . abbrev-mode)
         (prog-mode . abbrev-mode)))

;;; find-file-in-project

(use-package find-file-in-project
  :ensure t
  :bind (("C-. C-f" . find-file-in-project)
         ("C-. F" . find-file-in-project-by-selected)))

;;; aggressive-indent

(use-package aggressive-indent
  :ensure t
  :demand t
  :config
  (dolist (mode '(slime-repl-mode cperl))
    (add-to-list 'aggressive-indent-excluded-modes mode))
  (global-aggressive-indent-mode 1))

;;; electric-pair

(use-package electric-pair
  :hook (prog-mode . electric-pair-mode))

;;; rg

(use-package rg
  :ensure t
  :bind (("M-s d" . rg-dwim)
         ("M-s r" . rg)
         ("M-s p" . rg-project)
         ("M-s t" . rg-literal)))

;;; checkdoc

(use-package checkdoc
  :init
  (setq checkdoc-package-keywords-flag t))

;;; packages end here


;;; Text scaling, window resizing

;;; copied from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(require 'windmove)

(defun fyi-move-splitter-left (arg)
  "Move window splitter left.
Argument ARG number of units."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun fyi-move-splitter-right (arg)
  "Move window splitter right.
Argument ARG number of units."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun fyi-move-splitter-up (arg)
  "Move window splitter up.
Argument ARG number of units."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun fyi-move-splitter-down (arg)
  "Move window splitter down.
Argument ARG number of units."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))


;;; check for parens after save

(defun check-parens-hook ()
  "Hook that check for parens in Lisp modes."
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
    (check-parens)))

(add-hook 'after-save-hook #'check-parens-hook)


;;; custom narrowing
;;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p))
         (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t)
                )
               (t
                (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t
         (narrow-to-defun))))

(bind-key "n" 'narrow-or-widen-dwim ctl-x-map)


;;; in-place scrolling
;;; this means scroll wihtout moving the point. This is usually best when the point is centered.

(defun scroll-down-inplace ()
  "Scroll down but leave point where it is."
  (interactive)
  (scroll-down '(4)))

(defun scroll-up-inplace ()
  "Soll up but leave point where it is."
  (interactive)
  (scroll-up '(4)))


;;; Load machine-local configuration file

(let* ((sys-name (system-name))
       (machine-rc (concat "~/.emacs." sys-name)))
  (when (file-exists-p machine-rc)
    (load machine-rc)))


;;; enable disabled commands
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
