(require-package 'geiser)

(setq geiser-active-implementations '(guile))
(setq geiser-racket-binary "/usr/local/bin/guile")

(add-hook 'geiser-repl-mode-hook (lambda ()
                                   (paredit-mode 1)))

(provide 'init-geiser)
