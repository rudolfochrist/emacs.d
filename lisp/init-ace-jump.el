(require-package 'ace-jump-mode)

(setq ace-jump-mode-scope 'frame)

(setq ace-jump-mode-submode-list
     '(ace-jump-word-mode
       ace-jump-line-mode
       ace-jump-char-mode))

(global-set-key (kbd "C-M-S-q") 'ace-jump-mode)

(provide 'init-ace-jump)
