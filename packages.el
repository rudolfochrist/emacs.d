
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
  :bind (("C-c ,," . company-complete)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-h" . company-show-doc-buffer)
         ("M-." . company-show-location))
  :init
  (add-hook 'after-init-hook 'global-company-mode))

