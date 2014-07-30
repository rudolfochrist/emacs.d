(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp .t)
                                                         (js . t)
                                                         (R . t)
                                                         (sql . t)
                                                         (C . t)
                                                         (clojure . t)
                                                         (lisp . t)
                                                         (latex . t)
                                                         (python . t)
                                                         (ruby . t)
                                                         (scheme . t)
                                                         (sh . t)
                                                         ))

(setq org-confirm-babel-evaluate nil)   ; Don't ask for permission
(setq org-src-fontify-natively t)       ; Show syntax highlighting per language native mode in *.org
(setq org-src-preserve-indentation t)   ; For languages with significant whitespace like Python


(provide 'setup-org-babel)
