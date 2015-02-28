(require-package 'magit)
(require-package 'magit-gh-pulls)
(require-package 'git-timemachine)

;; Use the right emacs client --> https://github.com/magit/magit/issues/862
(when (window-system)
  (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))

;; magit status
(global-set-key (kbd "<f9>") 'magit-status)

;;; git-timemachine
(global-set-key (kbd "<M-f9>") 'git-timemachine)

(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(provide 'init-magit)
