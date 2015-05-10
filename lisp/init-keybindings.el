;; more conveinient backwar-kill-word
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; revert buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;;; always use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; single M-x key
(global-set-key (kbd "C-M-w") 'execute-extended-command)

(provide 'init-keybindings)
