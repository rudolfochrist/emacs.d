(require-package 'ag)
(require-package 'wgrep-ag)

(setq ag-reuse-buffers t
      ag-reuse-window t)

(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(provide 'init-ag)
