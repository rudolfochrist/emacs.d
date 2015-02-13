(require-package 'ag)

(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(global-set-key (kbd "<f2>") 'ag)

(provide 'init-ag)
