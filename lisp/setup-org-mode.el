(let ((org-dir (file-name-directory (locate-library "org"))))
  (unless (string-match "\.emacs\.d\/elpa\/org.*" org-dir)
    (package-install 'org-plus-contrib)))
(require-package 'org)
(require 'ox-latex)

(setq org-directory "~/org/")
(setq org-agenda-files '("~/org/todo.org"))

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "TODAY(o)" "WAITING(w)" "DEFERRED(f)" "|" "DONE(d)")
        (sequence "CANCELLED(c)")))
(setq org-log-done 'time)
(setq org-agenda-ndays 10)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-start-on-weekday nil)

;;; org-capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n")))

;;; setup diary
(setq diary-file (expand-file-name "~/org/diary"))
(setq org-agenda-include-diary t)

;;; custom agenda views
(setq org-agenda-custom-commands '(("d" "Today"
                                    ((agenda "" ((org-agenda-ndays 1)))
                                     (todo "TODAY")))))

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

(add-to-list 'org-latex-packages-alist '("" "color" nil))


(setq org-startup-indented t)           ; Use clean view
(setq org-startup-with-latex-preview t) ; Preview Latex Inline

;;; display inline images on startup
(setq org-image-actual-width '(450))
(setq org-startup-with-inline-images t)

;;; plotting with gnuplot
(require 'org-plot)
(local-set-key (kbd "C-M-g") 'org-plot/gnuplot)

;;; colorize/highlight text
;;; see: https://www.mail-archive.com/emacs-orgmode@gnu.org/msg29988.html
;; org-mode color
(org-add-link-type
 "color" nil
 (lambda (path desc format)
  (cond
   ((eq format 'html)
    (format "<span style=\"color:%s;\">%s</span>" path desc))
   ((eq format 'latex)
    (format "{\\color{%s}%s}" path desc)))))
;; org-mode highlight
(org-add-link-type
 "hl" nil
 (lambda (path desc format)
  (cond
   ((eq format 'html)
    (format "<font style=\"background-color:%s;\">%s</font>" path desc))
   ((eq format 'latex)
    (format "\\colorbox{%s}{%s}" path desc))))) ;; require \usepackage{color}


(provide 'setup-org-mode)
