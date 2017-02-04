
;;; avy

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)))



;;; ace-link

(use-package ace-link
  :config
  (ace-link-setup-default)
  ;; setup gnus
  (add-hook 'gnus-summary-mode-hook
            (lambda ()
              (bind-key "M-o" 'ace-link-gnus 'gnus-summary-mode-map)))
  (add-hook 'gnus-article-mode-hook
            (lambda ()
              (bind-key "M-o" 'ace-link-gnus 'gnus-article-mode-map)))  )


;;; ag

(use-package ag
  :init
  (setq ag-reuse-buffers t
        ag-reuse-window t)
  :config
  (use-package wgrep-ag
    :commands (wgrep-ag-setup))
  (add-hook 'ag-mode-hook 'wgrep-ag-setup))


;;; company-mode

(use-package company
  :bind (("C-c ,," . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-." . company-show-location))
  :config
  (add-hook 'after-init-hook 'global-company-mode))

