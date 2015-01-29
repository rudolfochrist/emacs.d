(require-package 'geiser)

(setq geiser-active-implementations '(racket))
(setq geiser-racket-binary "/usr/local/bin/racket")

(add-hook 'geiser-repl-mode-hook (lambda ()
                                   (paredit-mode 1)))

(provide 'init-geiser)
