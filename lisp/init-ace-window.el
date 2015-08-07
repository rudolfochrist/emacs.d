(require-package 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-always t)
(ace-window-display-mode 1)

(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-x p") 'aw-flip-window)

(provide 'init-ace-window)
