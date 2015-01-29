(require-package 'ag)

(setq ag-highlight-search t)
(setq ag-reuse-buffers t)

(defun fyi/ag-with-current-directory (search-string)
  (interactive "sSearch string: ")
  (let ((buffer-file-name (buffer-file-name)))
    (if buffer-file-name
        (ag search-string (file-name-directory buffer-file-name))
      (message "Cannot determine directory for buffer! Use 'ag' directly."))))
(global-set-key (kbd "<f2>") 'fyi/ag-with-current-directory)

(provide 'init-ag)
