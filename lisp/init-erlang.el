(add-to-list 'exec-path "/usr/local/opt/erlang/bin")
(setq erlang-root-dir "/usr/local/otp/erlang")
(require-package 'erlang-start
                 :noerror t
                 :from-dir
                 (car
                  (file-expand-wildcards
                   "/usr/local/opt/erlang/lib/erlang/lib/tools-*/emacs")))

;;; distel -- see https://github.com/massemanet/distel/blob/master/INSTALL
(require-package 'distel
                 :noerror t
                 :from-dir "/usr/local/share/distel/elisp/")
(with-eval-after-load "distel"
  (distel-setup))

(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))

(provide 'init-erlang)
