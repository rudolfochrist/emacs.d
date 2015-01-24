
;;; see also http://oremacs.com/2015/01/17/setting-up-ediff/
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")

(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(provide 'setup-ediff)
