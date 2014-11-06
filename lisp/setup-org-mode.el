(let ((org-dir (file-name-directory (locate-library "org"))))
  (unless (string-match "\.emacs\.d\/elpa\/org.*" org-dir)
    (package-install 'org-plus-contrib)))
(require-package 'org)
(require 'ox-latex)

(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/todo.org"))

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "DEFERRED(f)" "|" "DONE(d)")
        (sequence "CANCELLED(c)")))
(setq org-log-done 'time)
(setq org-agenda-ndays 10)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)

;;; org-capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n %t\n")))

;;; setup diary
(setq diary-file (expand-file-name "~/org/diary"))
(setq org-agenda-include-diary t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (set-input-method 'german-prefix)))

;;; latex export customization
(add-to-list 'org-latex-classes '("fyi-article"
                                  "\\documentclass[11pt,a4paper]{article}
                                  [DEFAULT-PACKAGES]
                                  [PACKAGES]
                                  \\usepackage{titling}
                                  \\usepackage{fancyhdr}
                                  [EXTRA]
                                  \\pagestyle{fancy}
                                  \\lhead{\\thetitle}
                                  \\rhead{\\theauthor}
                                  \\renewcommand{\\headrulewidth}{0.0}"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


(setq org-startup-indented t)           ; Use clean view
(setq org-startup-with-latex-preview t) ; Preview Latex Inline

;;; display inline images on startup
(setq org-image-actual-width '(450))
(setq org-startup-with-inline-images t)

;;; plotting with gnuplot
(require 'org-plot)
(local-set-key (kbd "C-M-g") 'org-plot/gnuplot)

(provide 'setup-org-mode)
