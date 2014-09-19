(require-package 'yasnippet)

(yas-reload-all)

(defun enable-yas-minor-mode ()
  (yas-minor-mode))

(add-hook 'prog-mode-hook 'enable-yas-minor-mode)
(add-hook 'html-mode-hook 'enable-yas-minor-mode)
(add-hook 'web-mode-hook 'enable-yas-minor-mode)
(add-hook 'org-mode-hook 'enable-yas-minor-mode)

(provide 'setup-yasnippet)
