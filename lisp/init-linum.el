
;;; Setup line numbers
(require-package 'linum-relative)
(global-linum-mode 1)
(setq linum-relative-current-symbol "")

;;; disable linum in some major modes
(add-to-list 'load-path (expand-file-name "linum-off" emacs-d-vendor))
(require 'linum-off)
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode dired-mode doc-view-mode image-mode))

(provide 'init-linum)
