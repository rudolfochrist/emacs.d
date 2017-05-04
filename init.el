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
      system-uses-terminfo nil                ; use Emacs terminfo
      backup-inhibited t                      ; disable backups
      auto-save-default nil                   ; no auto-save files
      ring-bell-function 'ignore              ; don't ring the bell
      scroll-preserve-screen-position 'always
      confirm-kill-emacs 'yes-or-no-p)

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
  (exec-path-from-shell-initialize))

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;;; disable GUI stuff
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; default theme with custom background
;;; see http://irreal.org/blog/?p=3900
(set-background-color "white smoke")
(add-to-list 'default-frame-alist '(background-color . "white smoke"))

;;; frame setup
(add-to-list 'default-frame-alist '(width . 140))
(add-to-list 'default-frame-alist '(height . 55))

;;; font's
;;; https://www.emacswiki.org/emacs/SetFonts
(defun select-font-candidate (&rest fonts)
  "Selects the first installed font among FONTS."
  (find-if (lambda (font)
             (find-font (font-spec :name font)))
           fonts))

(when (display-graphic-p)
  (set-face-attribute 'default nil :font
                      (select-font-candidate "Input-12")))

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
(which-function-mode 1)

;;; make scripts executable if shebang present
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; macOS lacks a compose key, so we're setting the input-method
(when (eql 'darwin system-type)
  (add-hook 'text-mode-hook
            (lambda ()
              (set-input-method "latin-9-prefix"))))



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
(bind-key "C-. f" #'toggle-frame-fullscreen)

;;; bury buffers instead of killing (that's so mean....)
;;; and most of the time I realized that I need the buffer again after killing it.
(defun bury-or-kill-buffer (arg)
  (interactive "P")
  (if arg
      (call-interactively #'kill-buffer)
    (bury-buffer)))
(bind-key "C-x k" #'bury-or-kill-buffer)

;;; reverting
(defun revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t))
(bind-key "C-c C-r" #'revert-buffer-no-confirm)


;;; libs

(use-package f     :defer t :load-path "site-lisp/f")
(use-package s     :defer t :load-path "site-lisp/s")
(use-package dash  :defer t :load-path "site-lisp/dash")
(use-package async :defer t :load-path "site-lisp/async")
(use-package hydra :load-path "site-lisp/hydra")


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
  :bind (("C-. \\" . toggle-en-de-dictionary))
  :preface
  (defun toggle-en-de-dictionary ()
    (interactive)
    (let ((dict ispell-current-dictionary))
      (cond
       ((equal dict "english")
        (ispell-change-dictionary "german8"))
       ((equal dict "german8")
        (ispell-change-dictionary "english")))))
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
  :bind (("C-S-s" . avy-goto-word-1)))



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


;;; ag

(use-package ag
  :load-path "site-lisp/ag"
  :commands (ag ag-regexp ag-project ag-project-regexp)
  :bind (("C-. S" . hydra-ag/body))
  :preface
  (defhydra hydra-ag (:color blue)
    ("s" ag-project "Search project")
    ("r" ag-regexp "Search regexp")
    ("p" ag-project-regexp "Search project regexp")
    ("q" nil "quit"))
  :init
  (setq ag-reuse-buffers t
        ag-reuse-window t)
  :config
  (use-package wgrep-ag
    :load-path "site-lisp/wgrep"
    :commands (wgrep-ag-setup)
    :init
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))


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
  :init
  (add-hook 'after-init-hook 'global-company-mode))


;;; dired

(use-package dired
  :load-path "site-lisp/dired"
  :commands (dired-jump)
  :bind (("C-. d" . dired-jump)
         ("C-. D" . dired-jump-elsewhere)
         :map dired-mode-map
         ("M-!" . async-shell-command)
         ("l" . dired-up-alternate-directory)
         ("RET" . dired-find-alternate-file)
         ("M-RET" . dired-open-natively))
  :preface
  (defun dired-jump-elsewhere ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'dired-jump)))
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
        dired-recursive-deletes 'top)
  (when (system-macos-p)
    (setq insert-directory-program "/usr/local/bin/gls"))
  :config
  (use-package dired-x)
  (use-package dired-details 
    :init
    ;; inspired by https://github.com/magnars/.emacs.d/
    (setq dired-details-hidden-string "--- ")
    :config
    (dired-details-install))
  (use-package dired-narrow
    :load-path "site-lisp/dired-hacks"
    :commands (dired-narrow)
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

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

  (defun eshell/clear ()
    "Clears the eshell buffer"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  :init
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")
              (add-to-list 'eshell-visual-commands "svn"))))


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
  :bind (("C-. C-'" . iedit-mode)))


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


;;; swiper, ivy, counsel

(use-package ivy
  :load-path "site-lisp/swiper"
  :demand t
  :preface
  (use-package smex
    :load-path "site-lisp/smex")
  ;; http://endlessparentheses.com/visit-directory-inside-a-set-of-directories.html
  (defvar ivy-prominent-directories
    '("~/prj/" "~/archive/" "~/.emacs.d/" "~/quicklisp/local-projects/")
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
  :bind (("C-. s" . counsel-ag)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("M-x" . counsel-M-x)
         ("C-x C-S-f" . counsel-recentf))
  :commands (counsel-more-chars))


;;; js2-mode

(use-package js2-mode
  :load-path "site-lisp/js2-mode" 
  :mode (("\\.js\\'" . js2mode)
         ("\\.json\\'" . js2mode))
  :init (setq js2-basic-offset 2))


;;; auctex

(use-package tex-site
  :load-path "site-lisp/auctex"
  :init
  (setq TeX-auto-save t
        TeX-parse-self t)
  :config
  (use-package latex-preview-pane
    :load-path "site-lisp/latex-preview-pane"
    :init (setq latex-preview-pane "xelatex")))


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
  :commands (magit-clone)
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
                                       ("~/prj/" . 1)
                                       ("~/quicklisp/local-projects/")))
  ;; install info
  (add-to-list 'Info-directory-list
               (expand-file-name "magit/Documentation" site-lisp-directory))
  :config
  (add-to-list 'magit-repolist-columns '("Dirty" 6 magit-repolist-column-dirty)) 
  (use-package magithub
    :load-path "site-lisp/magithub"
    :demand t
    :config (magithub-feature-autoinject t)))


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
  (defun paredit-adjust-spacing-p (endp delimiter)
    "Don't add space before splicing (,@) or reader macros."
    (cond
     ((or (looking-back "#\\+.*")
          (looking-back "#-.*"))
      t)
     ((looking-back "#.*")
      nil)
     ((looking-back ",@")
      nil)
     (t t)))
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
        whitespace-style '(face lines-tail tabs))
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
  :mode (("\\.asd\\'" . lisp-mode))
  :commands (slime)
  :bind (("C-. l" . switch-to-slime)
         ("C-. C-/" . slime-selector)
         :map slime-mode-map
         ("RET" . slime-autodoc-newline)
         ("C-c C-d s" . slime-documentation-in-minibuffer)
         ("C-c C-d i" . slime-insert-file-header)
         :map slime-repl-mode-map
         ("C-l" . slime-repl-clear-buffer)
         ("SPC" . slime-autodoc-space)
         ("C-j" . slime-autodoc-newline)
         ("C-c O" . slime-repl-inspect-last-expression)
         ("C-c C-d s" . slime-documentation-in-minibuffer))
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

  ;; docstring in minibuffer
  (defun slime-documentation-in-minibuffer (symbol)
    (interactive
     (let ((s-a-p (slime-symbol-at-point)))
       (cond
        ((or current-prefix-arg
             (not s-a-p))
         (list (slime-read-symbol-name "Symbol: ")))
        (t
         (list s-a-p)))))
    (let ((doc (slime-eval `(swank:documentation-symbol ,symbol))))
      ;; calculate if it fits into the echo areas
      ;; see: `display-message-or-buffer'
      (if (<= (count-lines-string doc)
              (typecase max-mini-window-height
                (integer max-mini-window-height)
                (float (* (frame-height)
                          max-mini-window-height))))
          (message "%s" doc)
        (slime-documentation symbol))))

  ;; switch to slime
  (defun switch-to-slime ()
    (interactive)
    (let ((slime-repl (find-if (lambda (buffer)
                                 (with-current-buffer buffer
                                   (when (eql 'slime-repl-mode major-mode)
                                     buffer)))
                               (buffer-list))))
      (if slime-repl
          (pop-to-buffer slime-repl)
        (call-interactively #'slime))))

  (defun slime-insert-file-header ()
    "Inserts the current file name into the buffer.

Later renditions should locate the asd file and insert the whole
subpath."
    (interactive)
    (let ((file-name (file-name-nondirectory (buffer-file-name))))
      (goto-char (point-min))
      (insert
       (format ";;; %s\n\n" file-name))))
  
  :init
  (setq slime-complete-symbol*-fancy t
        slime-complete-symbol-function #'slime-fuzzy-complete-symbol
        slime-startup-animation t
        slime-net-coding-system 'utf-8-unix
        slime-lisp-implementations'((ccl ("/usr/local/bin/ccl64"))
                                    (sbcl ("/usr/local/bin/sbcl"))
                                    (ecl ("/usr/local/bin/ecl"))
                                    (abcl ("/usr/local/bin/abcl"))))

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
    :commands (slime-company))

  ;; quicklisp REPL command
  (defslime-repl-shortcut slime-repl-quicklisp-quickload
    ("quicklisp-quickload" "ql")
    (:handler (lambda (&rest systems)
                (interactive (list (slime-read-system-name)))
                (insert (format "(ql:quickload '%s)" systems))
                (slime-repl-send-input t)))
    (:one-liner "cl:quickload system")))


;;; aggressive-indent

(use-package aggressive-indent
  :load-path "site-lisp/aggressive-indent"
  :demand t
  :config
  (dolist (mode '(slime-repl-mode feature-mode))
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


;;; paperless

(use-package paperless
  :load-path "~/prj/paperless"
  :commands (paperless)
  :init
  (setq paperless-capture-directory "~/PDF-CAPTURE"
        paperless-root-directory "~/archive"))


;;; smerge-mode

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


;;; enable disabled commands
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)

