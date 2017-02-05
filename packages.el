
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
  :init
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-visual-commands "htop")
              (add-to-lsit 'eshell-visual-commands "svn")))
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
  :bind (("C-. t" . switch-eshell)
         :map eshell-mode-map
         ("C-l" . eshell/clear)))


;;; feature-mode

(use-package feature-mode
  :mode "\\.feature\\'")


;;; goto-last-change

(use-package goto-last-change
  :bind (("C-. -" . goto-last-change)))


;;; hydra

(use-package hydra)


;;; iedit

(use-package iedit
  :bind* (("C-. '" . iedit-mode)))


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
               counsel-M-x))
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
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-repository-directories '(("~/.emacs.d/" . 0)
                                       ("~/prj/" . 1)))
  (use-package git-timemachine
    :commands (git-timemachine))
  :config
  (add-to-list 'magit-repolist-columns '("Dirty" 6 magit-repolist-column-dirty)))

