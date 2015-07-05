(require-package 'magit)
(require-package 'git-timemachine)

;; magit status
(global-set-key (kbd "<f9>") 'magit-status)

;;; git-timemachine
(global-set-key (kbd "<M-f9>") 'git-timemachine)

;;; use spelling in commit buffer
(add-hook 'git-commit-mode-hook (apply-partially 'fyi-configure-flyspell "en_US"))

(provide 'init-magit)
