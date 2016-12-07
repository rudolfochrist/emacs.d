(require-package 'auctex :load-only t)
(require-package 'latex-preview-pane)

(setq TeX-auto-save t
      TeX-parse-self t
      latex-preview-pane "xelatex")

(provide 'init-latex)
