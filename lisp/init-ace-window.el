(require-package 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-always t)
(ace-window-display-mode 1)
(global-set-key (kbd "C-x o") #'ace-window)

(defun fyi-switch-windows ()
  (interactive)
  (let ((window (aw--pop-window)))
    (if window
        (aw-switch-to-window window)
      (other-window 1))))

(when window-system
  (global-set-key (kbd "<C-return>") #'fyi-switch-windows))

(provide 'init-ace-window)
