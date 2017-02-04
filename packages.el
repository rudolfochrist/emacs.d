
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
    (dired-details-install)))
