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
			   ("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")))
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
      mac-command-modifier 'control
      user-full-name "Sebastian Christ"
      user-mail-address "rudolfo.christ@pm.me")

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;;; Use ISO calendar (YYYY-MM-DD)

(defvar init-last-local-map nil)

(defun do-insert-iso-timestamp ()
  (interactive)
  (calendar-exit)
  (use-local-map init-last-local-map)
  (let ((date (with-current-buffer "*Calendar*"
                (calendar-cursor-to-date t)))
        (hour (cl-parse-integer (read-from-minibuffer "Hour: ") :junk-allowed t))
        (minute (cl-parse-integer (read-from-minibuffer "Minute: ") :junk-allowed t)))
    (cl-destructuring-bind (month day year) date
      (insert (format-time-string
               "%FT%T%z"
               (encode-time 0
                            (or minute 0)
                            (or hour 0)
                            day
                            month
                            year
                            "GMT-2")
               "GMT-2")))))

(defun insert-iso-timestamp ()
  "Insert a ISO 8601 timestamp."
  (interactive)
  (setq init-last-local-map (current-local-map))
  (calendar)
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key (kbd "RET") #'do-insert-iso-timestamp))

(use-package calendar
  :config (calendar-set-date-style 'iso))

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
  :config
  (setq ivy-display-style 'fancy)
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-. B" . counsel-bookmark)))

;;; info

(setq Info-directory-list
      (append (list (expand-file-name "~/info"))
              Info-default-directory-list))

(use-package info
  :commands (info)
  :bind (("C-h a" . apropos)
         ("C-h A" . apropos))
  :demand t)

(use-package info-look
  :after info
  :bind (("C-h S" . info-lookup-symbol)))


;;; flyspell

(use-package flyspell
  :unless (eq window-system 'w32)
  :commands (flyspell-prog-mode flyspell-mode)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;;; flycheck

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

;;; org-mode

(use-package dot-org
  :load-path ("site-lisp"))

;;; wgrep

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-change)
  :config (setq wgrep-auto-save-buffer t))


;;; company-mode

(use-package company
  :ensure t
  :commands (global-company-mode)
  :bind (("C-. C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :hook (after-init . global-company-mode))


;;; dired

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

(use-package dired
  :bind (("C-x D" . dired)
         :map
         dired-mode-map
         ("M-!" . async-shell-command)
         ("l" . dired-up-alternate-directory)
         ("RET" . dired-find-alternate-file)
         ("M-RET" . dired-open-natively))
  :config
  (setq dired-garbage-files-regexp
        "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'"
        dired-listing-switches "-alvh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-dwim-target t))

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
  :config
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ;; ignores certain changes
        ediff-diff-options "-w"))


;;; iedit

(defun iedit-mode-defun ()
  (interactive)
  (iedit-mode '(0)))

(use-package iedit
  :ensure t
  :bind (("C-. C-'" . iedit-mode)
         ("C-. C-\"" . iedit-mode-defun)))

;;; js2-mode

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2
        js-indent-level 2))


;;; macrostep

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
              ("C-c M-e" . macrostep-expand)))


;;; magit

(defun magit-status-with-prefix ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'magit-status)))

(use-package magit
  :ensure t
  :commands (magit-clone magit-blame)
  :bind (("C-. gg" . magit-status)
         ("C-. gi" . magit-init)
         ("C-. gc" . magit-clone)
         ("C-. GG" . magit-status-with-prefix)
         ("C-. gl" . magit-list-repositories))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/code/" . 1)
                                       ("~/quicklisp/local-projects/" . 1)))
  ;; install info
  (add-to-list 'Info-directory-list
               (expand-file-name "magit/Documentation" site-lisp-directory))
  (add-to-list 'magit-repolist-columns '("Dirty" 6 magit-repolist-column-dirty)))


;;; paredit

(defvar paredit-non-space-patterns '("#\+" "#-" "#." ",@"))

(defun paredit-adjust-spacing-p (endp delimiter)
  "Don't add space before splicing (,@) or reader macros."
  (cl-notany #'looking-back paredit-non-space-patterns))

(use-package paredit
  :ensure t
  :commands (enable-paredit-mode)
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
  :hook (prog-mode . whitespace-mode)
  :config
  (setq whitespace-line-column 120
        whitespace-style '(face lines-tail tabs trailing)))


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
  :config
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

(use-package lisp-mode
  :mode "\\.lisp\\'"
  :mode "\\.cl\\'"
  :mode "\\.asd\\'")

(use-package dot-slime
  :load-path "site-lisp"
  :bind (("C-. l" . my-start-slime)))

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
  :config
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


(use-package imenu-anywhere
  :ensure t
  :after (imenu ivy)
  :bind (("C-. C-," . ivy-imenu-anywhere)))


;;; gnus

(use-package dot-gnus
  :disabled t
  :bind (("C-. m" . gnus)
         ("C-. M" . gnus-other-frame)))


;;; cperl-mode

;; https://www.emacswiki.org/emacs/CPerlMode#toc10
(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))

(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :mode "cpanfile"
  :interpreter (("perl" . cperl-mode)
                ("perl5" . cperl-mode)
                ("miniperl" . cperl-mode))
  :config
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
  (setq rm-whitelist " \\[.*\\]\\| st")
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
  :config
  (setq git-msg-prefix-input-method #'ivy-read))


;;; helpful

(use-package helpful
  :ensure t
  :bind ( ("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)
          ("C-h ." . helpful-at-point)))

;;; web-mode

(defun web-mode-lsp-bindings ()
  (bind-key "M-." #'slime-edit-definition web-mode-map)
  (bind-key "M-," #'slime-pop-find-definition-stack web-mode-map))

(use-package web-mode
  :ensure t
  :mode "\\.lsp\\'"
  :mode "\\.ep\\'"
  :mode "\\.html.erb\\'"
  :hook (web-mode . web-mode-lsp-bindings)
  :config
  (setq web-mode-auto-close-style 2))

;;; zel

(use-package zel
  :ensure t
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

(defun ffip-asd-project-root-p (dir)
  "Non-nil if DIR contains ASD files."
  (let ((asd (car (directory-files dir t "asd"))))
    (when asd
      (file-name-directory asd))))

(use-package find-file-in-project
  :ensure t
  :bind (("M-s M-f" . find-file-in-project)
         ("M-s F" . find-file-in-project-by-selected))
  :config
  (add-to-list 'ffip-project-file #'ffip-asd-project-root-p)
  (add-to-list 'ffip-ignore-filenames "*.fasl"))

;;; aggressive-indent

(use-package aggressive-indent
  :ensure t
  :demand t
  :config
  (dolist (mode '(slime-repl-mode cperl))
    (add-to-list 'aggressive-indent-excluded-modes mode))
  (global-aggressive-indent-mode 1))

;;; electric-pair

(defvar fyi-inhibit-electric-pair-modes '(org-mode))

(defun inhibit-electri-pair-mode-p (&rest _ignore)
  (not (member major-mode fyi-inhibit-electric-pair-modes)))

(use-package elec-pair
  :config
  (setq electric-pair-inhibit-predicate #'inhibit-electri-pair-mode-p))

;;; rg

(use-package rg
  :ensure t
  :bind (("M-s d" . rg-dwim)
         ("M-s r" . rg)
         ("M-s p" . rg-project)
         ("M-s t" . rg-literal)))

;;; checkdoc

(use-package checkdoc
  :config
  (setq checkdoc-package-keywords-flag t))

;;; anzu

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp)))

;;; german holidays

(use-package german-holidays
  :ensure t
  :demand t
  :config (setq calendar-holidays holiday-german-holidays))

;;; evil-swap-keys

(use-package evil-swap-keys
  :ensure t
  :commands (global-evil-swap-keys-mode)
  :hook ((lisp-mode . evil-swap-parens)
         (slime-repl-mode . evil-swap-parens)
         (emacs-lisp-mode . evil-swap-parens)
         (ielm-mode . evil-swap-parens)
         (eval-expression-minibuffer-setup . evil-swap-parens))
  :config (global-evil-swap-keys-mode))

(defun evil-swap-parens ()
  (evil-swap-keys-add-mapping "[" "(")
  (evil-swap-keys-add-mapping "]" ")")
  (evil-swap-keys-add-mapping "{" "[")
  (evil-swap-keys-add-mapping "}" "]")
  (evil-swap-keys-add-mapping "(" "{")
  (evil-swap-keys-add-mapping ")" "}"))

;;; ivy-xref

(use-package ivy-xref
  :ensure t
  :after xref
  :init (setq xref-show-definitions-function #'ivy-xref-show-defs))

;;; markdown

(use-package markdown-mode
  :ensure t)

;;; ledger

(use-package ledger-mode
  :ensure t
  :config
  (setq ledger-default-date-format ledger-iso-date-format
        ledger-reconcile-default-commodity "€"
        ledger-reconcile-insert-effective-date t))

;;;; ledger overwrites (I should fork this)

(defun ledger-do-reconcile (&optional sort target-date)
  "SORT the uncleared transactions in the account and display them in the *Reconcile* buffer.
Return a count of the uncleared transactions."
  (let* ((buf ledger-buf)
         (account ledger-acct)
         (sort-by (if sort
                      sort
                    "(date)"))
         (xacts
          (with-temp-buffer
            (if target-date
                (ledger-exec-ledger buf (current-buffer)
                                    "--uncleared" "--real" "emacs" "-e" target-date "--sort" sort-by account)
              (ledger-exec-ledger buf (current-buffer)
                                  "--uncleared" "--real" "emacs" "--sort" sort-by account))
            (goto-char (point-min))
            (unless (eobp)
              (if (looking-at "(")
                  (read (current-buffer))))))
         (fmt (ledger-reconcile-compile-format-string ledger-reconcile-buffer-line-format)))
    (if (> (length xacts) 0)
        (progn
          (if ledger-reconcile-buffer-header
              (insert (format ledger-reconcile-buffer-header account)))
          (dolist (xact xacts)
            (ledger-reconcile-format-xact xact fmt))
          (goto-char (point-max))
          (delete-char -1)) ;gets rid of the extra line feed at the bottom of the list
      (insert (concat "There are no uncleared entries for " account)))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)

    (length xacts)))


(defun ledger-reconcile-refresh ()
  "Force the reconciliation window to refresh.
Return the number of uncleared xacts found."
  (interactive)
  (let ((inhibit-read-only t)
        (line (count-lines (point-min) (point))))
    (erase-buffer)
    (prog1
        (ledger-do-reconcile ledger-reconcile-sort-key
                             (buffer-local-value 'ledger-end-date (current-buffer)))
      (set-buffer-modified-p t)
      (ledger-reconcile-ensure-xacts-visible)
      (goto-char (point-min))
      (forward-line line))))


(defun ledger-reconcile (&optional account target target-date)
  "Start reconciling, prompt for ACCOUNT."
  (interactive)
  (let ((account (or account (ledger-read-account-with-prompt "Account to reconcile")))
        (buf (current-buffer))
        (rbuf (get-buffer ledger-recon-buffer-name))
        (t-date (or target-date (ledger-read-date "End date"))))

    (when (ledger-reconcile-check-valid-account account)
      (if rbuf ;; *Reconcile* already exists
          (with-current-buffer rbuf
            (set 'ledger-acct account) ;; already buffer local
            (when (not (eq buf rbuf))
              ;; called from some other ledger-mode buffer
              (ledger-reconcile-quit-cleanup)
              (setq ledger-buf buf)) ;; should already be buffer-local

            (unless (get-buffer-window rbuf)
              (ledger-reconcile-open-windows buf rbuf)))

        ;; no recon-buffer, starting from scratch.

        (with-current-buffer (setq rbuf
                                   (get-buffer-create ledger-recon-buffer-name))
          (ledger-reconcile-open-windows buf rbuf)
          (ledger-reconcile-mode)
          (make-local-variable 'ledger-target)
          (set (make-local-variable 'ledger-buf) buf)
          (set (make-local-variable 'ledger-acct) account)
          (set (make-local-variable 'ledger-end-date) t-date)))

      (add-hook 'after-save-hook 'ledger-reconcile-refresh-after-save nil t)

      ;; Narrow the ledger buffer
      (if ledger-narrow-on-reconcile
          (ledger-occur (regexp-quote account)))

      (with-current-buffer rbuf
        (if (> (ledger-reconcile-refresh) 0)
            (ledger-reconcile-change-target target))
        (ledger-display-balance)))))

;;; skeletor

(use-package skeletor
  :ensure t
  :after ivy
  :config
  (setq skeletor-project-directory "~/code/"
        skeletor-completing-read-function #'ivy-completing-read
        skeletor-init-with-git nil))

(skeletor-define-template "lisp-init"
  :no-license? t
  :substitutions '(("__DESCRIPTION__" . (lambda () (read-string "Description: ")))))

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

;;; enable process killing in process list
;;; https://stackoverflow.com/questions/10627289/emacs-internal-process-killing-any-command

(defun fyi-kill-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

(bind-key "C-k" #'fyi-kill-process-at-point process-menu-mode-map)

;;; load machine-local configuration file

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

;;; got to $HOME
(cd "~")

(provide 'init)

;;; init.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp emacs-lisp-checkdoc)
;; End:
