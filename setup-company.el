(require-package 'company)
(require-package 'slime-company)

(setq company-idle-delay nil)           ; disable time delay
(add-hook 'after-init-hook 'global-company-mode) ; enable company in all modes
(global-set-key (kbd "C-SPC") 'company-complete)

(provide 'setup-company)
