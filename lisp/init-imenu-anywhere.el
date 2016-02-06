(require-package 'imenu-anywhere)

(defun fyi-imenu-anywhere-bindings ()
  (local-set-key (kbd "C-x m") #'imenu-anywhere))

(add-hook 'prog-mode-hook #'fyi-imenu-anywhere-bindings)

(provide 'init-imenu-anywhere)
