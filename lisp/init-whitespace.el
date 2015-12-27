(require 'whitespace)
(setq whitespace-line-column 120
      whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'init-whitespace)
