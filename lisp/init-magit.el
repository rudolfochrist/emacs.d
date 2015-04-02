(require-package 'magit)
(require-package 'git-timemachine)

(setq magit-last-seen-setup-instructions "1.4.0")

;; Use the right emacs client --> https://github.com/magit/magit/issues/862
(when (window-system)
  (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))

;; magit status
(global-set-key (kbd "<f9>") 'magit-status)

;;; git-timemachine
(global-set-key (kbd "<M-f9>") 'git-timemachine)

;;; use spelling in commit buffer
(add-hook 'git-commit-mode-hook (apply-partially 'fyi/configure-flyspell "en_US"))

;;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
(defun fyi/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-remote-config
         (magit-get-all "remote" "origin" "fetch")))
    (unless (or (null magit-remote-config)
                (member
                    fetch-address
                    magit-remote-config))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

(add-hook 'magit-mode-hook #'fyi/add-PR-fetch)

;;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html
(defun fyi/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
     (replace-regexp-in-string
      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
      (magit-get "remote"
                 (magit-get-current-remote)
                 "url"))
     (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'fyi/visit-pull-request-url))

(provide 'init-magit)
