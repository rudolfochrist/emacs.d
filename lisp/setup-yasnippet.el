(require-package 'yasnippet)

(yas-reload-all)

(defun enable-yas-minor-mode ()
  (yas-minor-mode))

(add-hook 'prog-mode-hook 'enable-yas-minor-mode)
(add-hook 'html-mode-hook 'enable-yas-minor-mode)
(add-hook 'web-mode-hook 'enable-yas-minor-mode)

;;; fix org-mode + yasnippet conflicts:
;;; http://stackoverflow.com/questions/9418148/conflicts-between-org-mode-and-yasnippet
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))
(add-hook 'org-mode-hook (lambda ()
                           (yas-minor-mode)
                           (make-variable-buffer-local 'yas/trigger-key)
                           (setq yas/trigger-key [tab}])
                           (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                           (define-key yas/keymap [tab] 'yas/next-field)))

(provide 'setup-yasnippet)
