(require-package 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-always t)
(ace-window-display-mode 1)
(global-set-key (kbd "C-x o") #'ace-window)

(when window-system
  (global-set-key (kbd "<C-return>") #'other-window))

(provide 'init-ace-window)
