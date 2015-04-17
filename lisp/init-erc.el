(require 'erc)

(setq erc-nick "rudolfochrist"
      erc-prompt-for-password nil
      erc-user-full-name "Sebastian (Rudolfo) Christ"
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#lisp" "#lispcafe"))
      erc-hide-list '("JOIN" "PART" "QUIT"))

(provide 'init-erc)
