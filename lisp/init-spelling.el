(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-list-command "list")
(setq ispell-extra-args '("--sug-mode=ultra"))

(ispell-change-dictionary "en_US")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'init-spelling)
