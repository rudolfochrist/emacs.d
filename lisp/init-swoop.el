(require-package 'swoop)

(define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)
(define-key swoop-map (kbd "C-r") 'swoop-pcre-from-swoop)

(provide 'init-swoop)
