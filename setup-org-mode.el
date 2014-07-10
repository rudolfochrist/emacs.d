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
(setq org-agenda-files '("~/Documents/todo.org"
                         "~/Documents/calendar.org"))

(defun add-zettle ()
  (interactive)
  (org-insert-heading)
  (insert (generate-zettel-id) " "))

(defun generate-zettel-id ()
  (time-fragments->string (extract-time-fragments (decode-time))))

(defun time-fragments->string (time-fragments)
  (reduce (lambda (m o)
            (concat m (number-to-string o)))
          time-fragments :initial-value ""))

(defun extract-time-fragments (decoded-time)
  (reverse (mapcar (lambda (idx)
             (nth idx decoded-time))
           '(1 2 3 4 5))))

(defun fyi/org-mode-keybindings ()
  (local-set-key (kbd "C-c z") 'add-zettle))

(add-hook 'org-mode-hook 'fyi/org-mode-keybindings)

(provide 'setup-org-mode)
