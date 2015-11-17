(require-package 'find-file-in-project)

(push "*.fasl" ffip-prune-patterns)

(global-set-key (kbd "C-x C-g") 'find-file-in-project)
(global-set-key (kbd "C-x G") 'find-file-in-project-by-selected)

(provide 'init-find-file-in-project)
