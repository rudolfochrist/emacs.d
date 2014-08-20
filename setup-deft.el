
(require-package 'deft)

(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Documents/References")

(global-set-key (kbd "C-c d d") 'deft)


(provide 'setup-deft)

