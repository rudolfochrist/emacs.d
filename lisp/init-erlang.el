(require-package 'erlang)

(setq erlang-root-dir "/usr/local/Cellar/erlang/17.1")
(setq exec-path (cons "/usr/local/Cellar/erlang/17.1/bin" exec-path))
(require 'erlang-start)

(defvar inferior-erlang-prompt-timeout t)

(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))

(provide 'init-erlang)
