(add-to-list 'load-path (car (file-expand-wildcards "/usr/local/opt/erlang/lib/erlang/lib/tools-*/emacs")))
(add-to-list 'exec-path "/usr/local/opt/erlang/bin")
(setq erlang-root-dir "/usr/local/otp/erlang")
(require 'erlang-start nil t)

;;; distel -- see https://github.com/massemanet/distel/blob/master/INSTALL
(add-to-list 'load-path "/usr/local/share/distel/elisp/")
(require 'distel nil t)
(with-eval-after-load "distel"
  (distel-setup))

(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))

(provide 'init-erlang)
