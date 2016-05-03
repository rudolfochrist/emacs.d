(require-package 'writeroom-mode)

(setq writeroom-global-effects (remove 'writeroom-set-fullscreen
                                       writeroom-global-effects))

(defun writeroom-disable-linum ()
  (cond
   (writeroom-mode
    (linum-mode -1)
    (writeroom-adjust-width 30))
   (t
    (linum-mode 1))))
(add-hook 'writeroom-mode-hook #'writeroom-disable-linum)

(provide 'init-writeroom-mode)
