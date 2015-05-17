(require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(require-package 'js-comint)

(setenv "NODE_DISABLE_COLORS" "1")
(setenv "NODE_NO_READLINE" "1")
(setq inferior-js-program-command "/usr/local/bin/node")

(defun fyi-js-key-bindings ()
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
  (local-set-key (kbd "C-c C-l") 'js-load-file-and-go))

(add-hook 'js2-mode-hook 'fyi-js-key-bindings)

(provide 'init-javascript)
