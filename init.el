;;; My emacs config.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; use CL features
(require 'cl-lib)

;;; site-lisp setup
(setq  site-lisp-directory (expand-file-name "site-lisp/" user-emacs-directory))
(add-to-list 'load-path site-lisp-directory)
(let ((default-directory site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

;;; prefer newer files
(setq load-prefer-newer t)

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; setup paths
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;; setup use-package
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

(setq default-input-method "latin-9-prefix")
(defun input-method-hook ()
  (if (eql 'darwin system-type)
      (set-input-method default-input-method)
    (set-input-method nil)))
(add-hook 'text-mode-hook #'input-method-hook)
(add-hook 'prog-mode-hook (lambda () (set-input-method nil)))



;;; UTILITIES

(defun window-system-p ()
  "Returns non-nil if current Emacs is running with a window-system."
  window-system)

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
(defvar ctl-period-map nil)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." #'ctl-period-map)

;;; easier other-window
(bind-key* "<C-return>" #'other-window)

;;; killing the whole region
(bind-key "C-x C-k" #'kill-region)

;;; buffer switching
(bind-key "C-x C-b" #'ivy-switch-buffer)
(bind-key "C-c C-b" #'ibuffer)

;;; fullscreen
(bind-key "C-x F" #'toggle-frame-fullscreen)

;;; bury buffers instead of killing (that's so mean....)
;;; and most of the time I realized that I need the buffer again after killing it.
(bind-key "C-x k" #'bury-buffer)


;;; libs

(use-package f    :defer t)
(use-package d    :defer t)
(use-package dash :defer t)


;;; avy

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)))



;;; ace-link

(use-package ace-link
  :commands (ace-link-gnus)
  :init
  ;; setup gnusn
  (add-hook 'gnus-article-mode-hook
            (lambda ()
              (bind-key "M-o" 'ace-link-gnus 'gnus-article-mode-map)))
  (add-hook 'gnus-summary-mode-hook
            (lambda ()
              (bind-key "M-o" 'ace-link-gnus 'gnus-summary-mode-map))) 
  :config
  (ace-link-setup-default))


;;; ag

(use-package ag
  :init
  (setq ag-reuse-buffers t
        ag-reuse-window t)
  :config
  (use-package wgrep-ag
    :commands (wgrep-ag-setup)
    :init
    (add-hook 'ag-mode-hook 'wgrep-ag-setup)))


;;; company-mode

(use-package company
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
    :commands (dired-narrow)
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

;;; easy-kill

(use-package easy-kill
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
    :init
    (setq erc-log-channels-directory "~/.erc/logs/"
          erc-save-buffer-on-part t)
    :config
    (erc-log-enable)))


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
              (add-to-lsit 'eshell-visual-commands "svn"))))


;;; feature-mode

(use-package feature-mode
  :mode "\\.feature\\'")


;;; goto-last-change

(use-package goto-last-change
  :bind (("C-. -" . goto-last-changhe)))


;;; hydra

(use-package hydra)


;;; iedit

(use-package iedit
  :bind (("C-. '" . iedit-mode)))


;;; interleave

(use-package interleave
  :init (setq interleave-org-notes-dir-list nil))


;;; irony-mode

(use-package irony
  :bind (:map irony-mode-map
              ([remap completion-at-point] . irony-completion-at-point-async)
              ([remap complete-symbol] . irony-completion-at-point-async))
  :config
  (dolist (hook '(c++-mode-hook objc-mode-hook c-mode-hook))
    (add-hook hook #'irony-mode))
  (use-package company-irony
    :commands (company-irony)
    :init (add-to-list 'company-backends #'company-irony))
  (use-package irony-eldoc
    :commands (irony-eldoc)
    :init (add-hook 'irony-mode-hook #'irony-eldoc)))


;;; gdb

(use-package gdb-mi
  :init (setq gdb-many-windows t))


;;; swiper, ivy, counsel

(use-package ivy
  :preface
  (require 'recentf)
  (use-package smex)
  (use-package counsel
    :commands (counsel-ag
               counsel-describe-function counsel-describe-variable
               counsel-M-x
               counsel-more-chars))
  ;; http://endlessparentheses.com/visit-directory-inside-a-set-of-directories.html
  (defvar ivy-prominent-directories
    '("~/PR/" "~/archive/" "~/.emacs.d/" "~/quicklisp/local-projects/")
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

  :bind (("C-. s" . counsel-ag)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-. i" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-d" . ivy-visit-prominent-directory))
  :init
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1))


;;; js2-mode

(use-package js2-mode
  :mode (("\\.js\\'" . js2mode)
         ("\\.json\\'" . js2mode)))


;;; auctex

(use-package auctex
  :init
  (setq TeX-auto-save t
        TeX-parse-self t)
  :config
  (use-package latex-preview-pane
    :init (setq latex-preview-pane "xelatex")))


;;; ledger-mode

(use-package ledger-mode
  :mode "\\.ledger\\'"
  :init
  (setq ledger-use-iso-dates t
        ledger-reconcile-default-commodity "EUR"
        ledger-reconcile-default-date-format "%Y-%m-%d"))


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
  :config
  (global-linum-mode 1))


;;; macrostep

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c M-e" . macrostep-expand)))


;;; magit

(use-package magit
  :bind (("C-. gg" . magit-status)
         ("C-. GG" . magit-status-with-prefix)
         ("C-. gl" . magit-list-repositories)
         ("C-. gt" . git-timemachine))
  :preface
  (defun magit-status-with-prefix ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'magit-status)))
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/prj/" . 1)))
  (use-package git-timemachine
    :commands (git-timemachine))
  :config
  (add-to-list 'magit-repolist-columns '("Dirty" 6 magit-repolist-column-dirty))
  (use-package magithub
    :config (magithub-feature-autoinject t)))


;;; markdown-mode

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode))
  :init
  (add-hook 'markdown-mode-hook #'visual-line-mode))


;;; multiple-cursors

(use-package multiple-cursors
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
  :config (global-page-break-lines-mode))


;;; paredit

(use-package paredit
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
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  :config
  (add-to-list 'paredit-space-for-delimiter-predicates #'paredit-adjust-spacing-p))


;;; redshank

(use-package redshank-loader
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
  :commands (undo-tree-visualize))


;;; which-key

(use-package which-key
  :config (which-key-mode))


;;; whitespace

(use-package whitespace
  :init
  (setq whitespace-line-column 120
        whitespace-style '(face lines-tail tabs))
  (add-hook 'prog-mode-hook #'whitespace-mode))


;;; yaml-mode

(use-package yaml-mode)


;;; yasnippet

(use-package yasnippet
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'yas-minor-mode))
  :config
  (yas-reload-all))


;;; ztree

(use-package ztree
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
  :mode (("\\.asd\\'" . lisp-mode))
  :bind (:map
         slime-mode-map
         ("RET" . slime-autodoc-newline)
         ("C-c C-d s" . slime-documentation-in-minibuffer)
         :map
         slime-repl-mode-map
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
          slime-banner slime-asdf slime-company
          slime-tramp slime-xref-browser slime-highlight-edits
          slime-sprof slime-macrostep slime-indentation))
  
  (add-hook 'lisp-mode-hook #'slime-mode)
  (add-hook 'inferior-lisp-mode-hook #'inferior-slime-mode)
  (add-hook 'lisp-mode-hook
            (lambda ()
              (setq-local lisp-indent-function #'common-lisp-indent-function)))
  :config
  ;; HyperSpec/Documentation
  (load (expand-file-name "~/quicklisp/clhs-use-local.el") t)
  (use-package quicklisp-docs
    :load-path "~/quicklisp/local-projects/quicklisp-docs/"
    :init (setq ql-docs-browser-function #'eww-browse-url)
    :config (ql-docs-reload-docs))

  ;; company backend
  (use-package slime-company
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
  :config
  (dolist (mode '(slime-repl-mode feature-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))
  (global-aggressive-indent-mode 1))


;;; imenu-anywhere

(use-package imenu-anywhere
  :bind (:map prog-mode-map
              ("C-. C-," . imenu-anywhere)))


;;; info

(use-package info
  :commands (info)
  :bind (("C-h a" . apropos)
         ("C-h A" . apropos))
  :init
  (setq Info-additional-directory-list '("~/info/"))
  :config
  (use-package info-look
    :bind (("C-h S" . info-lookup-symbol))))


;;; bbdb

(use-package bbdb
  :commands (bbdb bbdb-create)
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
  :config
  (bbdb-initialize 'gnus 'message 'anniv))

(use-package bbdb-vcard
  :after bbdb
  :commands (bbdb-vcard-export bbdb-vcard-import-file))


;;; gnus

(use-package dot-gnus
  :bind ("C-. m" . start-gnus))


;;; org-mode

(use-package org-mode)


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

