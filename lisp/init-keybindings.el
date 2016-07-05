;; more conveinient backwar-kill-word
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; revert buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;;; always use ibuffer
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c C-b") 'ibuffer)

;;; toggle fullscreen
(global-set-key (kbd "C-x t f") 'toggle-frame-fullscreen)

(provide 'init-keybindings)
