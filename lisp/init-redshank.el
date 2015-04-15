(require-package 'redshank)
(require 'redshank-loader)

(eval-after-load "redshank-loader"
  `(redshank-setup '(lisp-mode-hook
                     slime-repl-mode-hook) t))

(provide 'init-redshank)
