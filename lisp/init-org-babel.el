(require-package 'gnuplot)

(org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp .t)
                                                         (js . t)
                                                         (lisp . t)
                                                         (emacs-lisp . t)
                                                         (latex . t)
                                                         (python . t)
                                                         (ruby . t)
                                                         (sh . t)
                                                         (dot . t)
                                                         (gnuplot . t)
                                                         (ditaa . t)
                                                         (plantuml . t)))

(setq org-confirm-babel-evaluate nil)   ; Don't ask for permission
(setq org-src-fontify-natively t        ; Show syntax highlighting per language native mode in *.org
      org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)   ; For languages with significant whitespace like Python
(setq org-plantuml-jar-path "~/.emacs.d/bin/plantuml.8022.jar")
(setq org-ditaa-jar-path "~/.emacs.d/bin/ditaa0_9.jar")

(provide 'init-org-babel)
