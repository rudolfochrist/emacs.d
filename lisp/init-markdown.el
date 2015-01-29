(require-package 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))

(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (set-input-method 'german-prefix)))

(provide 'init-markdown)
