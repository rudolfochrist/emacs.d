(require-package 'magit)
(require-package 'magithub)
(require-package 'git-timemachine)

;; magit status
(global-set-key (kbd "<f9>") 'magit-status)

;;; git-timemachine
(global-set-key (kbd "<M-f9>") 'git-timemachine)

(provide 'init-magit)
