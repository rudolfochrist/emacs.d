(require 'info-look)
;;; Add custom documentation
(add-to-list 'Info-directory-list "~/Documents/info")

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

(provide 'setup-info)
