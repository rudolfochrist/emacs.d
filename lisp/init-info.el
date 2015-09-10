(require 'info-look)
;;; Add custom documentation
(add-to-list 'Info-directory-list "~/Documents/info")

(global-set-key (kbd "C-h a") #'apropos)
(global-set-key (kbd "C-h A") #'info-apropos)

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^() \t\n\"]+"
 :ignore-case t
 :doc-spec '(("(ansicl/ansicl)Symbol Index" nil nil nil)))

(info-lookup-add-help
 :mode 'slime-repl-mode
 :regexp "[^() \t\n\"]+"
 :ignore-case t
 :doc-spec '(("(ansicl/ansicl)Symbol Index" nil nil nil)))

(provide 'init-info)
