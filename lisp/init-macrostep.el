(require-package 'macrostep)

(define-key emacs-lisp-mode-map (kbd "C-c M-e") #'macrostep-expand)

(provide 'init-macrostep)
