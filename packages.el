
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
  :init (setq iedit-toggle-key-default (kbd "C-. '")))


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

