
(mapc (lambda (mode)
        (add-hook mode 'hl-line-mode))
      '(dired-mode-hook
        gnus-summary-mode-hook))

(provide 'init-hl-line-mode)
