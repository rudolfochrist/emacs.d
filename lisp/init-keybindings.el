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

;;; always do regex search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;;; ansi-term
(defun fyi/ansi-term-zsh ()
  "Runs ansi-term with zsh"
  (interactive)
  (ansi-term "/bin/zsh"))
(global-set-key (kbd "<M-f1>") 'fyi/ansi-term-zsh)

(provide 'init-keybindings)
