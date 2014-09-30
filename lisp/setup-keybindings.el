;; more conveinient backwar-kill-word
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; revert buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;;; use ElectricHelp as default
;;; see: http://www.emacswiki.org/emacs/ElectricHelp
(require 'ehelp)
(global-set-key (kbd "C-h") 'ehelp-command)

;;; always use hippie expand
(global-set-key (kbd "M-/") 'hippie-expand)

;;; always use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; dired jump to directory
(global-set-key (kbd "C-x C-j") 'dired-jump)

(provide 'setup-keybindings)
