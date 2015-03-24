(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'init-whitespace)
