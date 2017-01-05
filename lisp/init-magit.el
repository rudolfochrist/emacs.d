(require-package 'magit)
(require-package 'magithub)
(require-package 'git-timemachine)

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      magit-repository-directories '(("~/.emacs.d/" . 0)
                                     ("~/dev/" . 1)))

;; magit status
(global-set-key (kbd "<f9>") #'magit-status)
(global-set-key (kbd "<M-f9>") #'magit-list-repositories)

(provide 'init-magit)
