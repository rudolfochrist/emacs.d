
;;; Setup line numbers
(require-package 'linum-relative)
(global-linum-mode 1)
(setq linum-relative-current-symbol "")

;;; disable linum in some major modes
(load "linum-off/linum-off")
(setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode dired-mode doc-view-mode image-mode))

(provide 'init-linum)
