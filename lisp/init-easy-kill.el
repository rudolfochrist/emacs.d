(require-package 'easy-kill)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(provide 'init-easy-kill)
