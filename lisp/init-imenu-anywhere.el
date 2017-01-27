(require-package 'imenu-anywhere)

(defun fyi-imenu-anywhere-bindings ()
  (local-set-key (kbd "C-c ..") #'imenu-anywhere))

(add-hook 'prog-mode-hook #'fyi-imenu-anywhere-bindings)

(provide 'init-imenu-anywhere)
