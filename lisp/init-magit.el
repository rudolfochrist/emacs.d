(require-package 'magit)
(require-package 'git-timemachine)

;; Use the right emacs client --> https://github.com/magit/magit/issues/862
(when (window-system)
  (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))

;; magit status
(global-set-key (kbd "<f9>") 'magit-status)

;;; git-timemachine
(global-set-key (kbd "<M-f9>") 'git-timemachine)

;;; use spelling in commit buffer
(add-hook 'git-commit-mode-hook (apply-partially 'fyi/configure-flyspell "en_US"))

(provide 'init-magit)
