;;; My emacs config.

;; Added by Package.el.  This t come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; use CL features
(require 'cl-lib)

;;; load-path
(setq lisp-directory (expand-file-name "lisp" user-emacs-directory)
      site-lisp-directory (expand-file-name "site-lisp" user-emacs-directory))

(add-to-list 'load-path lisp-directory)
(add-to-list 'load-path site-lisp-directory)
(add-to-list 'load-path (expand-file-name "use-package" site-lisp-directory))

;;; prefer newer files
(setq load-prefer-newer t)

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; setup use-package
(setq use-package-enable-imenu-support t)
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
      checkdoc-package-keywords-flag t)

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
  :load-path "site-lisp/exec-path-from-shell"
  :demand t
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
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      require-final-newline t
      show-paren-style 'mixed)
(show-paren-mode 1)

;;; mode-line
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
  "Returns non-nil if `system-type' is 'darwin"
  (eql 'darwin system-type))

(defun mode-buffer-list (mode)
  "Returns a list of buffers with major mode MODE."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (eql major-mode mode)))
   (buffer-list)))

(defun lookup-binding (command)
  "Returns the keybinding for COMMAND."
  (substitute-command-keys (format "\\[%s]" command)))



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
  (interactive)
  (revert-buffer t t))
(bind-key "C-c C-r" #'revert-buffer-no-confirm)


;;; libs

(use-package f           :defer t :load-path "site-lisp/f")
(use-package s           :defer t :load-path "site-lisp/s")
(use-package dash        :defer t :load-path "site-lisp/dash")
(use-package async       :defer t :load-path "site-lisp/async")
(use-package popup       :defer t :load-path "site-lisp/popup")
(use-package list-utils  :defer t :load-path "site-lisp/list-utils")
(use-package loop        :defer t :load-path "site-lisp/loop")

;;; no deferred loading. I treat them as libs, but they aren't
;;; `require'd basically.
(use-package hydra :demand t :load-path "site-lisp/hydra")
(use-package xterm-color
  :load-path "site-lisp/xterm-color"
  :config (setenv "TERM" "xterm-256color"))


;;; swiper, ivy, counsel

(use-package smex
  :load-path "site-lisp/smex"
  :defer t)

(use-package ivy
  :load-path "site-lisp/swiper"
  :demand t
  :preface
  ;; http://endlessparentheses.com/visit-directory-inside-a-set-of-directories.html
  (defvar ivy-prominent-directories
    '("~/prj/" "~/code/" "~/archive/" "~/.emacs.d/" "~/quicklisp/local-projects/")
    "List of prominent directories.")

  (defun ivy-visit-prominent-directory (show-files-p)
    "Offers all directories inside a set of directories.

See `ivy-prominent-directories' for the list of directories to use.
With prefix argument SHOW-FILES-P also offer to find files."
    (interactive "P")
    (let ((completions
           (mapcar #'abbreviate-file-name
                   (cl-remove-if-not
                    (if show-files-p #'file-readable-p
                      #'file-directory-p)
                    (apply #'append
                           (mapcar (lambda (x)
                                     (when (file-exists-p x)
                                       (directory-files
                                        (expand-file-name x)
                                        t "^[^\.].*" t)))
                                   ivy-prominent-directories))))))
      (dired
       (ivy-completing-read "Open directory: "
                            completions 'ignored nil ""))))

  :bind (("C-. C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         ("C-x C-d" . ivy-visit-prominent-directory))
  :init
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package counsel
  :load-path "site-lisp/swiper"
  :after ivy
  :bind (("C-. s" . counsel-rg)
         ("C-x C-g" . counsel-locate)
         ("M-x" . counsel-M-x))
  :commands (counsel-more-chars counsel-esh history)
  :init
  (setq counsel-rg-base-command "rg -i --no-heading --line-number --max-columns 150 --color never --hidden %s ."))


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
  :commands (flyspell-prog-mode flyspell-mode)
  :bind (("C-. ]" . toggle-en-de-dictionary))
  :preface
  (defun toggle-en-de-dictionary ()
    (interactive)
    (let ((dict ispell-current-dictionary))
      (cond
       ((equal dict "english")
        (ispell-change-dictionary "german8"))
       ((equal dict "german8")
        (ispell-change-dictionary "english"))
       (t
        (ispell-change-dictionary "english")))
      (message
       (format "Ispell dictionary set to: %s"
               ispell-current-dictionary))))
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))


;;; org-mode

(use-package dot-org
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


;;; avy

(use-package avy
  :load-path "site-lisp/avy"
  :bind (("C-S-s" . avy-goto-char-timer)
         ("M-g s" . avy-goto-char-timer)
         ("M-g M-s" . avy-goto-char-timer)
         ("M-g a" . avy-goto-line)
         ("M-g M-a" . avy-goto-line)))



;;; ace-link

(use-package ace-link
  :load-path "site-lisp/ace-link"
  :demand t
  :init
  (add-hook 'gnus-article-mode-hook
            (lambda ()
              (bind-key "M-o" #'ace-link-gnus gnus-article-mode-map)))
  (add-hook 'gnus-summary-mode-hook
            (lambda ()
              (bind-key "M-o" #'ace-link-gnus gnus-summary-mode-map)))
  :config
  (ace-link-setup-default))


;;; wgrep

(use-package wgrep-ag
  :load-path "site-lisp/wgrep"
  :commands (wgrep-change)
  :init (setq wgrep-auto-save-buffer t))


;;; company-mode

(use-package company
  :load-path "site-lisp/company"
  :commands (global-company-mode)
  :bind (("C-. C-." . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . company-show-doc-buffer)
         ("M-." . company-show-location))

  :preface
  (defun company-bbdb-alias-candidates (candidates-fn arg)
    "Advice to resolve candidates for company-bbdb.

We're adding mail-aliases as well here. 

ARG is the one arguments taken by company bbdb candiates function."
    (let ((candidates (funcall candidates-fn arg))
          (aliases (eval '(bbdb-get-mail-aliases))))
      (append (cl-remove-if-not (lambda (alias)
                                  (string-match arg alias))
                                aliases)
              candidates)))
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (advice-add 'company-bbdb--candidates :around #'company-bbdb-alias-candidates))


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
  :load-path "site-lisp/dired-hacks"
  :after dired
  :commands (dired-narrow)
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-details
  :load-path "site-lisp/dired-details"
  :after dired
  :init
  (setq dired-details-initially-hide t
        dired-details-hide-link-targets nil
        dired-details-hidden-string " --- ")
  :config
  (dired-details-install))

(use-package find-dired
  :bind (("C-. w" . find-name-dired)))


;;; easy-kill

(use-package easy-kill
  :load-path "site-lisp/easy-kill"
  :bind (([remap kill-ring-save] . easy-kill)))


;;; ediff

(use-package ediff
  :init
  (setq ediff-window-setup-function #'ediff-setup-windows-plain
        ediff-split-window-function #'split-window-horizontally
        ;; ignores certain changes
        ediff-diff-options "-w"))


;;; erc

(use-package erc
  :commands (erc erc-tls)
  :init
  (setq erc-nick "rudolfochrist"
        erc-prompt-for-password nil
        erc-user-full-name "Sebastian (Rudolfo) Christ"
        erc-autojoin-channels-alist '(("freenode.net"
                                       "#lisp" "#lispcafe" "#sbcl" "#ccl"))
        erc-hide-list '("JOIN" "PART" "QUIT")
        erc-fill-function #'erc-fill-static
        erc-fill-static-center 22
        ;; Freenode TLS port
        erc-port 6697)
  :preface
  ;;; see: http://endlessparentheses.com/marking-emacs-chat-buffers-as-read.html
  (defun erc-mark-as-read ()
    "Mark buffer as read up to current line."
    (interactive)
    (let ((inhibit-read-only t))
      (put-text-property (point-min)
                         (line-beginning-position)
                         'face
                         'font-lock-comment-face)))
  :bind (:map erc-mode-map
              ("<escape>" . erc-mark-as-read))
  :config
  (use-package erc-log
    :commands (erc-log-enable)
    :init
    (setq erc-log-channels-directory "~/.erc/logs/"
          erc-save-buffer-on-part t))
  (erc-log-enable))


;;; eshell

(use-package eshell
  :bind (("C-. t" . switch-eshell))
  :preface
  (defun switch-eshell ()
    "Switch to eshell buffer or hide it if current buffer"
    (interactive)
    (if (eq major-mode 'eshell-mode)
        (switch-to-buffer (second (buffer-list)))
      (eshell)))

  (defun eshell/cls ()
    "Clears the eshell buffer by recenter to top."
    (interactive)
    (goto-char (point-max))
    (recenter-top-bottom 1))

  (defun setup-eshell-hook ()
    ;; xterm-colors
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    ;; visual commands
    (push "htop" eshell-visual-commands)
    (push "svn" eshell-visual-commands)
    (push "perldoc" eshell-visual-commands)
    (push "aws" eshell-visual-commands)
    (push "ncdu" eshell-visual-commands)
    (push '("git" "log") eshell-visual-subcommands)
    (push '("docker" "pull" "push") eshell-visual-subcommands)
    (push '("docker" "-it") eshell-visual-options)
    (push '("brew" "install") eshell-visual-subcommands)
    ;; key overwrites
    (bind-key "C-l" 'eshell/cls eshell-mode-map)
    (bind-key "M-r" 'counsel-esh-history eshell-mode-map))

  (defun eshell-hook ()
    (setq xterm-color-preserve-properties t)
    (eshell-cmpl-initialize))
  :init
  (setq eshell-ls-use-colors t
        eshell-hist-ignoredups t
        eshell-destroy-buffer-when-process-dies t
        eshell-scroll-show-maximum-output nil)
  (add-hook 'eshell-first-time-mode-hook #'setup-eshell-hook)
  (add-hook 'eshell-mode-hook #'eshell-hook))

(use-package eshell-prompt-extras
  :load-path "site-lisp/eshell-prompt-extras"
  :after eshell
  :commands (epe-theme-lambda)
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function #'epe-theme-lambda))

;;; https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-eshell.el#L171
(use-package bash-completion
  :load-path "site-lisp/emacs-bash-completion"
  :after eshell
  :preface
  (defun eshell-bash-completion ()
    (while (pcomplete-here
            (nth 2
                 (bash-completion-dynamic-complete-nocomint
                  (save-excursion
                    (eshell-bol)
                    (point))
                  (point))))))
  :init
  (setq bash-completion-nospace t
        eshell-default-completion-function #'eshell-bash-completion))



;;; feature-mode

(use-package feature-mode
  :load-path "site-lisp/cucumber"
  :mode "\\.feature\\'")


;;; goto-last-change

(use-package goto-last-change
  :load-path "site-lisp/goto-last-change"
  :bind (("C-. C--" . goto-last-change)))


;;; iedit

(use-package iedit
  :load-path "site-lisp/iedit"
  :bind (("C-. C-'" . iedit-mode)
         ("C-. C-\"" . iedit-mode-defun))
  :preface
  (defun iedit-mode-defun ()
    (interactive)
    (iedit-mode '(0))))


;;; interleave

(use-package interleave
  :load-path "site-lisp/interleave"
  :init (setq interleave-org-notes-dir-list nil))


;;; irony-mode

(use-package irony
  :load-path "site-lisp/irony"
  :bind (:map irony-mode-map
              ([remap completion-at-point] . irony-completion-at-point-async)
              ([remap complete-symbol] . irony-completion-at-point-async))
  :config
  (dolist (hook '(c++-mode-hook objc-mode-hook c-mode-hook))
    (add-hook hook #'irony-mode))
  (use-package company-irony
    :load-path "site-lisp/company-irony"
    :demand t
    :init (add-to-list 'company-backends #'company-irony))
  (use-package irony-eldoc
    :load-path "site-lisp/irnoy-eldoc"
    :demand t
    :init (add-hook 'irony-mode-hook #'irony-eldoc)))


;;; gdb

(use-package gdb-mi
  :init (setq gdb-many-windows t))


;;; js2-mode

(use-package js2-mode
  :load-path "site-lisp/js2-mode" 
  :mode (("\\.js\\'" . js2-mode)
         ("\\.json\\'" . js2-mode))
  :preface
  (defun enable-json-lax-semi-warnings ()
    (when (string= (file-name-extension (buffer-file-name))
                   "json")
      (setq js2-strict-missing-semi-warning nil)))
  :init
  (setq js2-basic-offset 2
        js-indent-level 2)
  (add-hook 'js2-mode-hook #'enable-json-lax-semi-warnings))


;;; auctex

(use-package tex-site
  :load-path "site-lisp/auctex"
  :init
  (setq TeX-auto-save t
        TeX-parse-self t))

(use-package latex-preview-pane
  :load-path "site-lisp/latex-preview-pane"
  :init (setq latex-preview-pane "xelatex"))


;;; ledger-mode

(use-package ledger-mode
  :load-path "site-lisp/ledger-mode"
  :mode "\\.ledger\\'"
  :init
  (setq ledger-use-iso-dates t
        ledger-reconcile-default-commodity "EUR"
        ledger-reconcile-default-date-format "%Y-%m-%d")
  (add-hook 'ledger-report-mode-hook #'hl-line-mode))


;;; linum

(use-package linum
  :disabled t
  :preface
  (defun disable-linum-on-huge-file ()
    "Disable linum if buffer contains more than 5000 lines.

This requires wc to be installed. Uses wc -c file for performace reason.

Ref: http://blog.binchen.org/posts/turn-off-linum-mode-when-file-is-too-big.html"
    (when (and (executable-find "wc")
               (> (string-to-number
                   (shell-command-to-string (format "wc -c %s" (buffer-file-name))))
                  (* 5000 80))
               (linum-mode -1))))
  :init
  (add-hook 'prog-mode-hook #'disable-linum-on-huge-file)
  (add-hook 'text-mode-hook #'disable-linum-on-huge-file)
  (add-hook 'eshell-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'slime-repl-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'gnus-topic-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'gnus-summary-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'gnus-article-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'org-agenda-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'doc-view-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'paperless-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'slime-inspector-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'slime-fuzzy-completions-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'Man-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'ledger-report-mode-hook (lambda () (linum-mode -1)))
  :config
  (global-linum-mode 1))


;;; macrostep

(use-package macrostep
  :load-path "site-lisp/macrostep"
  :bind (:map emacs-lisp-mode-map
              ("C-c M-e" . macrostep-expand)))


;;; magit

(use-package git-timemachine
  :load-path "site-lisp/git-timemachine"
  :bind (("C-. gt" . git-timemachine)))

(use-package magit
  :load-path "site-lisp/magit/lisp"
  :commands (magit-clone magit-blame)
  :bind (("C-. gg" . magit-status)
         ("C-. GG" . magit-status-with-prefix)
         ("C-. gl" . magit-list-repositories))
  :preface
  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  (use-package with-editor
    :load-path "site-lisp/with-editor"
    :demand t)
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

(use-package ssh-agency
  :load-path "site-lisp/ssh-agency"
  :if (eq window-system 'w32)
  :after magit)


;;; markdown-mode

(use-package markdown-mode
  :load-path "site-lisp/markdown-mode"
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode))


;;; multiple-cursors

(use-package multiple-cursors
  :load-path "site-lisp/multiple-cursors"
  :commands (mc/edit-lines
             mc/mark-all-like-this
             mc/mark-next-like-this
             mc/skip-to-next-like-this
             mc/unmark-next-like-this
             mc/mark-previous-like-this
             mc/skip-to-previous-like-this
             mc/unmark-previous-like-this
             mc/insert-numbers
             mc/insert-letters)
  :bind (("C-<" . mc-hydra/body))
  :init
  (defhydra mc-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit

[_i_] Insert numbers \(optional number prefix\)
[_I_] Insert letter \(optinoal number prefix\)
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
("i" mc/insert-numbers)
("I" mc/insert-letters)
("q" nil)))


;;; page-break-lines

(use-package page-break-lines
  :load-path "site-lisp/page-break-lines"
  :config (global-page-break-lines-mode))


;;; paredit

(use-package paredit
  :load-path "site-lisp/paredit"
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
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates #'paredit-adjust-spacing-p))

(use-package paredit-everywhere
  :load-path "site-lisp/paredit-everywhere"
  :commands (paredit-everywhere-mode)
  :init
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))



;;; redshank

(use-package redshank-loader
  :load-path "site-lisp/redshank-loader"
  :bind (:map redshank-mode-map
              ("C-. v" . redshank-hydra/body))
  :preface
  (defun redshank-let<->let* ()
    (interactive)
    (save-excursion
      (redshank-point-at-enclosing-let-form)
      (forward-char)
      (cond
       ((looking-at "let ")
        (forward-word)
        (insert "*"))
       ((looking-at "let\\* ")
        (forward-word)
        (delete-char 1)))))

  (defhydra hydra-redshank (:color blue)
    "Modifications"
    ("c" redshank-condify-form "Condify")
    ("e" redshank-eval-whenify-form "Eval-Whenify")
    ("f" redshank-complete-form "Complete form")
    ("l" redshank-letify-form-up "Letify up")
    ("L" redshank-enclose-form-with-lambda "Enclose with λ")
    ("k" redshank-let<->let* "LET <-> LET*")
    ("n" redshank-rewrite-negated-predicate "Negate predicate")
    ("p" redshank-maybe-splice-progn "Maybe splice progn")
    ("x" redshank-extract-to-defun "Extract to defun")
    ("q" nil "Quit"))

  :config
  (redshank-setup '(lisp-mode-hook
                    emacs-lisp-mode-hook
                    slime-repl-mode-hook)))


;;; undo-tree

(use-package undo-tree
  :load-path "site-lisp/undo-tree"
  :commands (undo-tree-visualize))


;;; which-key

(use-package which-key
  :load-path "site-lisp/which-key"
  :config (which-key-mode))


;;; whitespace

(use-package whitespace
  :init
  (setq whitespace-line-column 120
        whitespace-style '(face lines-tail tabs trailing))
  (add-hook 'prog-mode-hook #'whitespace-mode))


;;; yaml-mode

(use-package yaml-mode
  :load-path "site-lisp/yaml-mode")


;;; yasnippet

(use-package yasnippet
  :load-path "site-lisp/yasnippet"
  :commands (yas-expand yas-minor-mode)
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'yas-minor-mode))
  :config
  (yas-reload-all))


;;; ztree

(use-package ztree
  :load-path "site-lisp/ztree"
  :commands (ztree-diff))


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
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

;;; slime

(use-package slime
  :load-path "site-lisp/slime"
  :mode (("\\.asd\\'" . lisp-mode)
         ("\\.cl\\'" . lisp-mode))
  :commands (slime-mode slime-repl-mode)
  :bind (("C-. l" . slime)
         ("C-. L" . slime-connect)
         ("C-. C-/" . slime-selector)
         :map slime-mode-map
         ("RET" . slime-autodoc-newline)
         ("C-c C-d i" . slime-insert-file-header)
         ("C-c C-d w" . slime-who-hydra/body)
         :map slime-repl-mode-map
         ("C-l" . slime-repl-clear-buffer)
         ("SPC" . slime-autodoc-space)
         ("C-j" . slime-autodoc-newline)
         ("C-c O" . slime-repl-inspect-last-expression)
         ("C-c C-d w" . slime-who-hydra/body))
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
  :init
  (setq slime-complete-symbol*-fancy t
        slime-complete-symbol-function #'slime-fuzzy-complete-symbol
        slime-startup-animation t
        slime-net-coding-system 'utf-8-unix
        slime-documentation-lookup-function #'slime-eww-hyperspec-lookup)

  ;; Lisps
  (setq slime-lisp-implementations
        '((ccl ("ccl64"))
          (sbcl ("sbcl"))
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

  (add-hook 'lisp-mode-hook #'slime-mode)
  (add-hook 'inferior-lisp-mode-hook #'inferior-slime-mode)
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
    :load-path "site-lisp/slime-company"
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
  (defhydra slime-who-hydra (:color blue :hint nil)
    ("q" nil)
    ("c" slime-who-calls "who calls")
    ("w" slime-calls-who "calls who")
    ("r" slime-who-references "who references")
    ("b" slime-who-binds "who-binds")
    ("s" slime-who-sets "who-sets")
    ("m" slime-who-macroexpands "who macroexpands")
    ("p" slime-who-specializes "who specializes"))
  ;; save the buffer after compilation
  (add-hook 'slime-compilation-finished-hook
            (lambda (&rest ignore)
              (save-buffer))))


;;; aggressive-indent

(use-package aggressive-indent
  :load-path "site-lisp/aggressive-indent"
  :demand t
  :config
  (dolist (mode '(slime-repl-mode feature-mode cperl dockerfile-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))
  (global-aggressive-indent-mode 1))


;;; imenu-anywhere

(use-package imenu-anywhere
  :load-path "site-lisp/imenu-anywhere"
  :bind (:map prog-mode-map
              ("C-. C-," . imenu-anywhere)))


;;; bbdb

(use-package bbdb-loaddefs
  :load-path "site-lisp/bbdb/lisp"
  :commands (bbdb bbdb-create bbdb-snarf)
  :init
  (setq bbdb-file "~/org/contacts.bbdb"
        bbdb-offer-save 'auto
        bbdb-notice-auto-save-file t
        bbdb-expand-mail-aliases t
        bbdb-canonicalize-redundant-nets-p t
        bbdb-always-add-addresses t
        bbdb-complete-mail-allow-cycling t
        bbdb-completion-display-record nil
        ;; don't handle anniversaries in BBDB.
        bbdb-anniv-alist nil)
  (add-to-list 'Info-directory-list
               (expand-file-name "bbdb/doc" site-lisp-directory))
  :config
  (bbdb-initialize 'gnus 'message 'anniv))

(use-package bbdb-vcard
  :load-path "site-lisp/bbdb-vcard"
  :after bbdb
  :commands (bbdb-vcard-export bbdb-vcard-import-file))


;;; gnus

(use-package dot-gnus
  :bind (("C-. m" . gnus)
         ("C-. M" . gnus-other-frame))
  :init
  (setq gnus-init-file (expand-file-name "dot-gnus" lisp-directory)))


;;; restclient

(use-package restclient
  :load-path "site-lisp/restclient"
  :commands (restclient-mode))


;;; smerge-mode

(use-package smerge-mode
  :commands (smerge-mode)
  :bind (:map
         smerge-mode-map
         ("C-c C-." . hydra-smerge-mode/body))
  :preface
  (defhydra hydra-smerge-mode ()
    "smerge commands"
    ("n" smerge-next "Next diff")
    ("p" smerge-prev "Previous diff")
    ("b" smerge-keep-base "Keep base")
    ("m" smerge-keep-mine "Keep mine")
    ("o" smerge-keep-other "Keep other")
    ("a" smerge-keep-all "Keep all")
    ("q" nil "quit" :color blue)))


;;; cperl-mode

(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
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


;;; dockerfile-mode

(use-package dockerfile-mode
  :load-path "site-lisp/dockerfile-mode"
  :mode "Dockerfile\\'" 
  :commands (dockerfile-mode))


;;; find-file-in-project

(use-package find-file-in-project
  :load-path "site-lisp/find-file-in-project"
  :bind (("C-x C-S-f" . find-file-in-project)
         ("C-. fd" . ffip-show-diff-by-description)
         ("C-. fD" . ffip-show-diff)
         ("C-. fc" . ffip-create-project-file)
         ("C-. fs" . fyi-save-all-project-buffers))
  :commands (ffip-project-root)
  :preface
  (defun fyi-save-all-project-buffers (path)
    (interactive (list (ffip-project-root)))
    (when (null path)
      (user-error "Cannot save project buffers outside of a project."))
    (let ((buffers (cl-remove-if-not (lambda (buffer)
                                       (cl-search (expand-file-name path)
                                                  (buffer-file-name buffer)))
                                     (buffer-list))))
      (mapc #'save-buffer buffers))))


;;; rich-minority

(use-package rich-minority
  :load-path "site-lisp/rich-minority"
  :demand t
  :init
  (setq rm-whitelist "\\[.*\\]")        ; just showing slime-mode: [package lisp-impl]
  :config
  (rich-minority-mode 1))


;;; dumb-jump

(use-package dumb-jump
  :load-path "site-lisp/dumb-jump"
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy))


;;; es-mode

(use-package spark :load-path "site-lisp/spark" :defer t)
(use-package requets :load-path "site-lisp/request" :defer t)

(use-package es-mode
  :load-path "site-lisp/es-mode"
  :mode "\\.es\\'"
  :commands (es-mode))

(use-package es-cc
  :load-path "site-lisp/es-mode"
  :commands (es-command-center)
  :after es-mode)


;;; sgml-mode

(use-package sgml-mode
  :commands (sgml-mode sgml-pretty-print))


;;; beginend

(use-package beginend
  :load-path "site-lisp/beginend"
  :config
  (beginend-global-mode))


;;; helm-dash

(use-package helm-dash
  :load-path "site-lisp/helm-dash"
  :after helm
  :bind (("C-. h" . helm-dash))
  :init (setq helm-dash-docsets-path
              (expand-file-name "~/docs/docsets/")
              helm-dash-common-docsets
              '("AllegroLisp")))


;;; editorconfig

(use-package editorconfig
  :load-path "site-lisp/editorconfig-emacs"
  :config
  (editorconfig-mode 1))


;;; commit-msg-prefix

(use-package commit-msg-prefix
  :load-path "site-lisp/commit-msg-prefix"
  :after magit
  :bind (:map
         ;; magit
         with-editor-mode-map
         ("C-. C-;" . commit-msg-prefix))
  :init
  (setq commit-msg-prefix-input-method #'ivy-read))


;;; pass

(use-package password-store
  :load-path "site-lisp/password-store")

(use-package pass
  :load-path "site-lisp/pass"
  :commands (pass))


;;; helpful

(use-package elisp-refs
  :load-path "site-lisp/elisp-refs"
  :defer t)

(use-package helpful
  :load-path "site-lisp/helpful"
  :bind ( ("C-h f" . helpful-function)
          ("C-h F" . helpful-macro)
          ("C-h v" . helpful-variable)
          ("C-h ." . helpful-at-point)))


;;; highlight-quoted
;; https://github.com/Fanael/highlight-quoted

(use-package highlight-quoted
  :load-path "site-lisp/highlight-quoted"
  :commands (highlight-quoted-mode)
  :init
  (add-hook 'lisp-mode-hook #'highlight-quoted-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))


;;; web-mode

(use-package web-mode
  :load-path "site-lisp/web-mode"
  :preface
  (defun web-mode-lsp-bindings ()
    (bind-key "M-." #'slime-edit-definition web-mode-map)
    (bind-key "M-," #'slime-pop-find-definition-stack web-mode-map))
  :init
  (add-to-list 'auto-mode-alist '("\\.lsp\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook #'web-mode-lsp-bindings))


;;; zel

(use-package a        :load-path "site-lisp/a")
(use-package frecency :load-path "site-lisp/frecency")

(use-package zel
  :load-path "~/code/zel"
  :demand t
  :bind (("C-x C-r" . zel-find-file-frecent))
  :config
  (zel-install))


;;; acw-window

(use-package ace-window
  :load-path "site-lisp/ace-window"
  :bind (("C-x O" . ace-window)))


;;; package-lint

(use-package package-lint
  :load-path "site-lisp/package-lint"
  :commands (package-lint-current-buffer
             package-lint-buffer))

;;; packages end here


;;; Text scaling, window resizing (hydra)

;;; copied from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-scaling (:hint nil)
  "
Some global mappings.
_q_ : I just changed my mind. Quit.

     Text Scaling                Window Resizing
-------------------------  ----------------------------
_g_: increase text scale              _k_
_f_: decrease text scale           _h_     _l_
_0_: default text size                _j_
                           _=_: Balance windows
"
  ("q" nil)
  ("g" text-scale-increase)
  ("f" text-scale-decrease)
  ("0" (text-scale-adjust 0) :color blue)
  ("h" (hydra-move-splitter-left 5))
  ("j" (hydra-move-splitter-down 5))
  ("k" (hydra-move-splitter-up 5))
  ("l" (hydra-move-splitter-right 5))
  ("=" balance-windows :color blue))

(bind-key "C-. =" #'hydra-scaling/body)


;;; search KB
;;; this has to be replaced real soon...

(defun counsel-fyi-kb-search-function (string &rest unused)
  "Lookup STRING with index-cli"
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (format "index-cli -i /Users/fyi/org -q \"%s\"" string))
    nil))

(defun search-kb (&optional initial-input)
  "Search KB"
  (interactive)
  (ivy-read "search KB: " 'counsel-fyi-kb-search-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action (lambda (x)
                      (when (string-match ".*\\[\\(.*\\)|\\([[:digit:]]+\\)\\]" x)
                        (let ((file-name (match-string 1 x))
                              (point (string-to-number (match-string 2 x))))
                          (find-file file-name)
                          (goto-char point)
                          (org-show-entry)
                          (show-children))))))

(bind-key "C-. k" #'search-kb)


;;; check for parens after save

(defun check-parens-hook ()
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
    (check-parens)))

(add-hook 'after-save-hook #'check-parens-hook)


;;; custom narrowing
;;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
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
