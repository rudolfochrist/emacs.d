(require 'erc)

(setq erc-nick "rudolfochrist"
      erc-prompt-for-password nil
      erc-user-full-name "Sebastian (Rudolfo) Christ"
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#lisp" "#lispcafe"))
      erc-hide-list '("JOIN" "PART" "QUIT"))

;;; enable logging
(push 'log erc-modules)
(setq erc-log-channels-directory "~/.erc/logs/"
      erc-save-buffer-on-part t)

(provide 'init-erc)
