(let ((org-dir (file-name-directory (locate-library "org"))))
  (unless (string-match "\.emacs\.d\/elpa\/org.*" org-dir)
    (package-install 'org-plus-contrib)))
(require-package 'org)

(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/todo.org" "~/org/cal.org"))

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "DEFERRED(f)" "|" "DONE(d)")))
(setq org-log-done 'time)
(setq org-agenda-ndays 10)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)

;;; org-capture
(global-set-key (kbd "C-M-/") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n")))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (set-input-method 'german-prefix)))


(setq org-startup-indented t)           ; Use clean view
(setq org-startup-with-latex-preview t) ; Preview Latex Inline

;;; display inline images on startup
(setq org-image-actual-width '(450))
(setq org-startup-with-inline-images t)

(provide 'setup-org-mode)