(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load "company"
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-.") 'company-show-location))

(provide 'init-company-mode)
