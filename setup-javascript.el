(require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;;; REPL glory
(require-package 'js-comint)
(setq inferior-js-program-command "/usr/local/bin/node")
(setenv "NODE_NO_READLINE" "1")

(defun fyi-js2-mode-keys () 
  "My js3-mode key bindings"
  (interactive)
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer)
  (local-set-key (kbd "C-c C-k") 'js-send-buffer-and-go)
  (local-set-key (kbd "C-c l") 'js-load-file-and-go))

(add-hook 'js2-mode-hook 'fyi-js2-mode-keys)


(provide 'setup-javascript)
