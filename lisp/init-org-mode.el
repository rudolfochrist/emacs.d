;;; org-export backends
;;; apperently this has to be set BEFORE org.el is loaded!
(setq org-export-backends '(ascii html latex texinfo))

(require-package 'org-plus-contrib
                 :require 'org)
(require 'ox-latex)
(require 'org-checklist)

;;; enable org-protocol
(unless (server-running-p)
  (server-start))
(require 'org-protocol)

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
(setq org-agenda-todo-ignore-deadlines 'near)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t) ;http://superuser.com/questions/501440/emacs-org-mode-how-to-avoid-duplicate-lines-in-agenda-when-items-is-scheduled

(defun fyi/summarize-captured-url ()
  "Summarizes the website for the given URL using sumy (https://github.com/miso-belica/sumy)."
  (let ((url (plist-get org-store-link-plist :link)))
    (with-temp-buffer
      (call-process "sumy" nil t nil "text-rank" (format "--url=%s" url) "--length=10%")
      (buffer-string))))

;;; org-capture
(global-set-key (kbd "<M-f12>") 'org-capture)
(setq org-capture-templates
      '(("t"
         "todo"
         entry
         (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n")
        ("r"
         "Add link to ~/org/read-later.org via org-protocol"
         item
         (file "~/org/read-later.org")
         "- [[%:link][%:description]] --- (%u)"
         :prepend t :immediate-finish t)
        ("b"
         "Add bookmark via org-protocol"
         entry
         (file "~/org/kb.org")
         "* %a :bookmark:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?\n\n** Website Summary:\n\n%(fyi/summarize-captured-url)\n\n"
         :prepend t :empty-lines 1)
        ("v"
         "Add bookmark via org-protocol but omit summary"
         entry
         (file "~/org/kb.org")
         "* %a :bookmark:\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n%?"
         :prepend t :empty-lines 1)))

;;; setup diary
(setq diary-file (expand-file-name "~/org/diary"))
(setq org-agenda-include-diary t)
(calendar-set-date-style 'iso)

;;; custom agenda views
(setq org-agenda-custom-commands '(("d" "Today"
                                    ((agenda "" ((org-agenda-ndays 1)))
                                     (todo "TODAY")))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)

;;; open read-later.org
(defun fyi/open-read-later ()
  (interactive)
  "Opens the ~/org/read-later.org file"
  (find-file (expand-file-name "~/org/read-later.org")))
(global-set-key (kbd "C-c C-r") 'fyi/open-read-later)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (set-input-method 'german-prefix)))

;;; Enable yasnippets in org
;;; from Richard Riley in org-mode mailing list
(add-hook 'org-mode-hook '(lambda ()
                            (make-variable-buffer-local 'yas/trigger-key)
                            (setq yas/trigger-key [tab])
                            (define-key yas/keymap [tab] 'yas/next-field-group)))

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

;;; needed for bibtex
(setq org-latex-pdf-process '("pdflatex -interaction nonstopmode %f"
                              "bibtex %b"
                              "pdflatex -interaction nonstopmode %f"
                              "pdflatex -interaction nonstopmode %f"))

;;; use Skim to open PDFs
(add-to-list 'org-file-apps '("\\.pdf\\'" . "/Applications/Skim.app/Contents/SharedSupport/displayline 1 %s"))

;;; org-download
(load "org-download/org-download")

(defun fyi/configure-org-download ()
  "Configures `org-download'."
  (setq org-download-image-dir "~/org/org-download-images/"
        org-download-heading-lvl nil
        org-download-image-width 200))

(add-hook 'org-mode-hook #'fyi/configure-org-download)

(provide 'init-org-mode)
