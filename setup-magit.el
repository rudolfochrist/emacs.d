(require-package 'magit)

;; Use the right emacs client --> https://github.com/magit/magit/issues/862
(when (window-system)
  (set-variable 'magit-emacsclient-executable "/usr/local/bin/emacsclient"))

(provide 'setup-magit)
