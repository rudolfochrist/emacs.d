;;; init.el --- My Emacs Initialization File  -*- lexical-binding: t; -*-
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
      ring-bell-function 'ignore       ; don't ring the bell
      scroll-preserve-screen-position 'always
      confirm-kill-emacs 'yes-or-no-p
      scroll-margin 2
      mac-command-modifier 'control
      mac-control-modifier 'super
      user-full-name "Sebastian Christ"
      user-mail-address "rudolfo.christ@proton.me")

;;; use GNU ls
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;;; Fonts & Typography

(set-frame-font "SF Mono 12" nil t)

;;; auto-save + backups

(defvar backup-dir (expand-file-name "backups/" user-emacs-directory))

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-directory-alist `((".*" . ,backup-dir))
      auto-save-file-name-transforms `((".*" ,backup-dir t)))

;;; Use ISO calendar (YYYY-MM-DD)

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
  (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "TERM" "CC" "PERL5LIB")))

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
(setq server-socket-dir "~/.emacs.d/server/")
(when (and (not (getenv "EMACS_NO_SERVER"))
           (not (server-running-p)))
  (server-start))


;;; KEYBINDINGS
;;;
;;; M-s is for search
;;; M-g is for goto
;;; C-. prefix map is for personal bindings

;;; C-.
(defvar ctl-period-map nil)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." #'ctl-period-map)
;;; fullscreen
(bind-key "C-. y" #'toggle-frame-fullscreen)

;;; reverting
(defun revert-buffer-no-confirm ()
  "Revert current buffer but don't nag."
  (interactive)
  (revert-buffer t t))
(bind-key "C-c C-r" #'revert-buffer-no-confirm)

;;; expanding

(bind-key "M-/" #'hippie-expand)

;;; zapping

(bind-key "M-z" #'zap-up-to-char)

;;; buffers
(bind-key "C-x b" #'list-buffers)
(bind-key "C-x C-b" #'switch-to-buffer)

;;; compiling
(bind-key "C-c C-," #'compile)

;;; expand current windows
(bind-key "s-1" #'delete-other-windows)
(bind-key "s-0" #'delete-window)

;;; open url
(bind-key "s-o" #'browse-url)

;;; libs

(use-package f        :defer t :ensure t)
(use-package s        :defer t :ensure t)
(use-package dash     :defer t :ensure t)

;;; completion
(require 'icomplete)
(fido-vertical-mode 1)

;;; corfu

(use-package corfu
  :ensure t
  :config
  (setq corfu-auto t
        corfu-quit-no-match t)
  (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;;; info

(use-package info
  :commands (info)
  :bind (("C-h a" . apropos)
         ("C-h A" . apropos))
  :demand t
  :config
  (setq Info-additional-directory-list
        (list (expand-file-name "~/info/"))))

(use-package info-look
  :after info
  :bind (("C-h S" . info-lookup-symbol)))


;;; org-mode

(use-package dot-org
  :load-path ("site-lisp"))

;;; wgrep

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-change)
  :config (setq wgrep-auto-save-buffer t))

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
         ("C-. C-\\" . iedit-mode-defun)))

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
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/code/" . 1)
                                       ("~/common-lisp/" . 1)))
  :config
  ;; install info
  (add-to-list 'Info-directory-list
               (expand-file-name "magit/Documentation" site-lisp-directory))
  (add-to-list 'magit-repolist-columns '("Dirty" 6 magit-repolist-column-dirty)))

;;; git-timemachine

(use-package git-timemachine
  :ensure t
  :bind (("C-. gt" . git-timemachine)))

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
         (minibuffer-setup . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (slime-repl-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode))
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates #'paredit-adjust-spacing-p)

  ;; using this for extended search
  (unbind-key "M-s" paredit-mode-map)
  ;; Version 25 changes
  ;; https://paredit.org/cgit/paredit/plain/NEWS
  (define-key paredit-mode-map (kbd "RET") nil)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline))

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

;;; lisp

(use-package lisp-mode
  :mode "\\.lisp\\'"
  :mode "\\.cl\\'"
  :mode "\\.asd\\'"
  :hook ((lisp-mode . slime-mode)))

(defun local/find-project-asd ()
  "Find the project ASD file."
  (interactive)
  (let ((asds (directory-files (project-root (project-current t)) t ".asd")))
    (cond
     ((zerop (length asds))
      (user-error "No ASD files found!"))
     ((= (length asds) 1)
      (find-file (car asds)))
     (t
      (find-file (completing-read "ASD file: " asds nil t))))))

;;; slime

(use-package slime
  :ensure t
  :bind (("C-. l" . slime)
         ("C-. C-/" . slime-selector))
  :init
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy
                         slime-mrepl
                         slime-banner
                         slime-asdf
                         slime-tramp
                         slime-xref-browser
                         slime-sprof)))

(with-eval-after-load 'slime-repl
  (bind-key "C-l" #'slime-repl-clear-buffer slime-repl-mode-map)

  (def-slime-selector-method ?a "Visit system definition (asd) buffer."
                             (local/find-project-asd)))

;;; sly

(use-package sly
  :disabled t
  :ensure t
  :bind (("C-. l" . sly)
         :map sly-selector-map
         ("a" . local/find-project-asd))
  :init
  (setq sly-lisp-implementations
        '((sbcl ("sbcl"))
          (sbcl-no-userinit ("sbcl" "--no-userinit"))
          (ecl ("ecl"))))
  :config
  (global-set-key (kbd "C-. C-/") sly-selector-map))

(with-eval-after-load 'sly-mrepl
  (bind-key "C-l" 'sly-mrepl-clear-repl sly-mrepl-mode-map)

  (defun reinitialize-asdf-source-registry ()
    (sly-mrepl--eval-for-repl '(rc:reinitialize-source-registry)))

  (advice-add 'sly-mrepl-set-directory :after #'reinitialize-asdf-source-registry)

  (defun uiopcwd ()
    (interactive)
    (sly-eval-async '(cl:namestring (uiop:getcwd))
                    (lambda (pwd)
                      (message "%s" pwd))))

  (add-to-list 'sly-mrepl-shortcut-alist '("pwd" . uiopcwd)))

(use-package sly-asdf
  :disabled t
  :ensure t
  :after sly)

(use-package sly-named-readtables
  :disabled t
  :ensure t
  :after sly)

(use-package sly-macrostep
  :disabled t
  :ensure t
  :after sly)

;;; lisp indentation rules

(use-package indentation-rules
  :load-path "site-lisp"
  :after slime)

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
  :bind (("C-. C-," . imenu))
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
  ;; almost cperl-hairy. Keep C-h f
  (setq cperl-font-lock t
        cperl-electric-lbrace-space t
        cperl-electric-parens t
        cperl-electric-linefeed t
        cperl-electric-keywords t
        cperl-lazy-help-time t)
  ;; better block indentation
  (setq cperl-indent-parens-as-block t
        cperl-close-paren-offset (- cperl-indent-level))
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

;;; sgml-mode

(use-package sgml-mode
  :commands (sgml-mode sgml-pretty-print))

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
  :mode "\\.html.erb\\'"
  :mode "\\.html\\'"
  :config
  (setq web-mode-auto-close-style 2))

(use-package emmet-mode
  :ensure t
  :after web-mode
  :hook (html-mode web-mode)
  :bind (:map web-mode-map
              ("<tab>" . emmet-power-tab)))

(defun emmet-power-tab (arg)
  (interactive "P")
  (emmet-expand-line arg)
  (indent-for-tab-command arg))

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

;;; project.el + super-t/command-t file finding

(use-package project
  :ensure t
  :commands (project-root
             project-files
             project-ignores
             project-external-roots)
  :config
  (add-to-list 'project-find-functions 'cl-project-search t))

(defun cl-project-search (start)
  (let ((asd-file (locate-dominating-file
                   start
                   (lambda (dir)
                     (let ((files (directory-files dir t "\\.asd")))
                       (unless (null files)
                         (car files)))))))
    (when asd-file
      (cons 'common-lisp asd-file))))

(cl-defmethod project-root ((project (head common-lisp)))
  (cdr project))

(defun super-t ()
  (interactive)
  (find-file (completing-read "File: " (project-files (project-current t)) nil t)))

(bind-key "s-t" 'super-t)

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

;;; evil-swap-keys

(use-package evil-swap-keys
  :ensure t
  :commands (global-evil-swap-keys-mode)
  :hook ((lisp-mode . evil-swap-parens)
         (slime-repl-mode . evil-swap-parens)
         (emacs-lisp-mode . evil-swap-parens)
         (ielm-mode . evil-swap-parens)
         (eval-expression-minibuffer-setup . evil-swap-parens)
         (sql-mode . evil-swap-parens)
         (ruby-mode . evil-swap-parens))
  :config (global-evil-swap-keys-mode))

(defun evil-swap-parens ()
  (evil-swap-keys-add-mapping "[" "(")
  (evil-swap-keys-add-mapping "]" ")")
  (evil-swap-keys-add-mapping "{" "[")
  (evil-swap-keys-add-mapping "}" "]")
  (evil-swap-keys-add-mapping "(" "{")
  (evil-swap-keys-add-mapping ")" "}"))

;;; markdown

(use-package markdown-mode
  :ensure t)

;;; skeletor

(use-package skeletor
  :load-path "site-lisp/skeletor.el"
  :config
  (setq skeletor-project-directory "~/code/"
        skeletor-completing-read-function 'completing-read))

(skeletor-define-template "lisp-base"
  :title "Common Lisp Base System"
  :substitutions '(("__DESCRIPTION__" . (lambda () (read-string "Description: "))))
  :no-license? t
  :after-creation (lambda (dir)
                    (skeletor-shell-command "./make.sh install_hooks" dir)))

(defun local/add-lisp-project-flavour (src)
  (interactive
   (list (completing-read "Template: " (f-directories skeletor-user-directory) nil t)))
  (let* ((cwd (file-name-directory (if (equal major-mode 'dired-mode)
                                       (dired-current-directory)
                                     (buffer-file-name))))
         (project-name (f-filename cwd))
         (flavour (cadr (s-split "-" (f-filename src))))
         (asd-file (s-concat flavour ".asd")))
    (f-entries src
               (lambda (f)
                 (if (string= (f-filename f) asd-file)
                     (f-append-text (s-replace-all (list (cons "__PROJECT-NAME__" project-name)) (f-read f))
                                    'utf-8-unix
                                    (f-join cwd (s-concat project-name ".asd")))
                   (let ((dest (f-join cwd (f-filename f))))
                     (f-write (s-replace-all (list (cons "__PROJECT-NAME__" project-name)) (f-read f))
                              'utf-8-unix
                              dest)
                     (set-file-modes dest (file-modes f))))))
    (when (equal major-mode 'dired-mode)
      (revert-buffer))))

;;; ztree

(use-package ztree
  :ensure t)

;;; sql-mode

(use-package sql
  :init
  (setq sql-product 'postgres))

;;; sql-upcase

(use-package sql-upcase
  :load-path "site-lisp"
  :hook ((sql-mode . sql-upcase-mode)))

;;; multiple-cursors

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-l" . mc/mark-all-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;;; undo-tree

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "backups/" user-emacs-directory))))
  :config (global-undo-tree-mode))

;;; jinx

(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;; crux

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-S-<return>" . crux-smart-open-line-above)
         ("S-<return>" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-x k" . bury-buffer)
         ("C-x K" . kill-buffer)
         ("M-K" . crux-kill-other-buffers)
         ("C-M-z" . crux-indent-defun)
         ("C-x I" . crux-find-user-init-file)
         ("M-o" . crux-switch-to-previous-buffer)
         ("C-g" . crux-keyboard-quit-dwim)
         ("s-j" . crux-top-join-line)
         ("C-c S" . crux-visit-shell-buffer)))

;;; expand-region

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))

;;; packages end here

;;; check for parens after save

(defun check-parens-hook ()
  "Hook that check for parens in Lisp modes."
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
    (check-parens)))

(add-hook 'after-save-hook #'check-parens-hook)

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

;;; colorize compilation buffer output
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

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
