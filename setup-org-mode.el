(require-package 'org)

(setq org-todo-keywords
      '((sequence "TODO" "HOLD" "DONE")))

(setq org-log-done 'time)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (set-input-method 'german-prefix)))

(provide 'setup-org-mode)
