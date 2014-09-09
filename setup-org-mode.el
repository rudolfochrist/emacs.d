(let ((org-dir (file-name-directory (locate-library "org"))))
  (unless (string-match "\.emacs\.d\/elpa\/org.*" org-dir)
    (package-install 'org)))
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

(setq org-startup-indented t)           ; Use clean view
(setq org-startup-with-latex-preview t) ; Preview Latex Inline

(provide 'setup-org-mode)
