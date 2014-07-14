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

(setq org-confirm-babel-evaluate nil)

(provide 'setup-org-babel)
