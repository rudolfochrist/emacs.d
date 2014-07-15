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

(setq org-agenda-files '("~/Documents"))

(defun add-zettle ()
  (interactive)
  (org-insert-heading)
  (insert (generate-zettel-id) " "))

(defun generate-zettel-id ()
  (format-time-string "%Y%m%d-%4N"))

(defun fyi/org-mode-keybindings ()
  (local-set-key (kbd "C-c C-n") 'add-zettle))

(add-hook 'org-mode-hook 'fyi/org-mode-keybindings)

(provide 'setup-org-mode)
