(require-package 'magit)
(require-package 'git-timemachine)

;; Use the right emacs client --> https://github.com/magit/magit/issues/862
(when (window-system)
  (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))

;; magit status
(global-set-key (kbd "C-c g s") 'magit-status)

;;; magit-log
(global-set-key (kbd "C-c g l") 'magit-log)

;;; git-timemachine
(global-set-key (kbd "C-c g h") 'git-timemachine)

(provide 'setup-magit)
