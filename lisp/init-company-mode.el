(require-package 'company)

(add-hook 'after-init-hook 'global-company-mode)
(defun fyi-company-keybindings ()
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "M-.") 'company-show-location))
(add-hook 'company-mode-hook #'fyi-company-keybindings)

(global-set-key (kbd "C-' C-'") #'company-complete)

(provide 'init-company-mode)
