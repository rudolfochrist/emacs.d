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

;;; move by paragprah
(global-set-key (kbd "M-[") #'backward-paragraph)
(global-set-key (kbd "M-]") #'forward-paragraph)

;;; go through frames
(global-set-key (kbd "C-`") #'other-frame)

;;; mostly I want to kill this buffer
;;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(global-set-key (kbd "C-x k") #'kill-this-buffer)

(provide 'init-keybindings)
