(require-package 'company)
(require-package 'slime-company)

(add-to-list 'slime-contribs 'slime-company)
(add-to-list 'company-backends 'company-slime)

(define-key company-active-map (kbd "M-.") 'company-show-location)

(add-hook 'after-init-hook 'global-company-mode)

(provide 'init-company-mode)
