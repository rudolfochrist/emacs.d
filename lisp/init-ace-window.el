(require-package 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(ace-window-display-mode 1)

(global-set-key (kbd "C-x o") 'ace-window)

(provide 'init-ace-window)
