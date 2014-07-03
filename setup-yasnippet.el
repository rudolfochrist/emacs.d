(require-package 'yasnippet)

(yas-reload-all)
(add-hook 'prog-mode-hook '(lambda ()
                             (yas-minor-mode)))

(provide 'setup-yasnippet)
