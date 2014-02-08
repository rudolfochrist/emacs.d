(require-package 'multiple-cursors)

(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "M-}") 'mc/mark-next-like-this)
(global-set-key (kbd "M-{") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-S-}") 'mc/mark-all-like-this)

(provide 'setup-multiple-cursors)
