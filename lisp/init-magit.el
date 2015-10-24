(require-package 'magit)
(require-package 'git-timemachine)

(require-package 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; magit status
(global-set-key (kbd "<f9>") 'magit-status)

;;; git-timemachine
(global-set-key (kbd "<M-f9>") 'git-timemachine)

(provide 'init-magit)
