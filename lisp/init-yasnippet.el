(require-package 'yasnippet)

(yas-reload-all)

(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)

(provide 'init-yasnippet)
