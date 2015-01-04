(require 'eshell)
(require 'em-smart)

;;; setup smart eshell
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(global-set-key (kbd "<f1>") 'eshell)

(provide 'setup-eshell)
