(require-package 'expand-region)

(global-set-key (kbd "C-\\") 'er/expand-region)

(defun contract-region ()
  "See http://stackoverflow.com/a/6156444"
  (interactive)
  (setq current-prefix-arg '-1)
  (call-interactively 'er/expand-region))
(global-set-key (kbd "C-M-\\") 'contract-region)

(provide 'init-expand-region)
