(require 'dired-x)




;;; dired-narrow
(require-package 'dired-narrow)

(with-eval-after-load "dired"
  (define-key dired-mode-map "/" #'dired-narrow))

(provide 'init-dired)
