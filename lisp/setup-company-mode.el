(require-package 'company)
(require-package 'slime-company)

(slime-setup '(slime-company))
(add-hook 'after-init-hook 'global-company-mode)

(provide 'setup-company-mode)
