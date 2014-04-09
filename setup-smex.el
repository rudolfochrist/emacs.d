(require-package 'smex)
(smex-initialize)

;; M-x wihout alt key and smex
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)

(provide 'setup-smex)
