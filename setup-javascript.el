(require-package 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

(require-package 'skewer-mode)
(setq phantomjs-program-name "/usr/local/bin/phantomjs")
(skewer-setup)

(provide 'setup-javascript)
