(require-package 'enh-ruby-mode)
(require-package 'inf-ruby)
(require-package 'bundler)
(require-package 'robe)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-robe))

(provide 'init-ruby)
