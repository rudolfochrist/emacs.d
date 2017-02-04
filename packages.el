
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
              (define-key gnus-summary-mode-map (kbd "M-o") #'ace-link-gnus)))
  (add-hook 'gnus-article-mode-hook
            (lambda ()
              (define-key gnus-article-mode-map (kbd "M-o") #'ace-link-gnus))))
