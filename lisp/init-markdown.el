(require-package 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

(add-hook 'markdown-mode-hook visual-line-mode)

(provide 'init-markdown)
