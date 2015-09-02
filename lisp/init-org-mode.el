;;; org-export backends
;;; apperently this has to be set BEFORE org.el is loaded!
(setq org-export-backends '(ascii html latex texinfo))

(require-package 'org-plus-contrib
                 :require 'org)
(require 'ox-latex)
(require 'ox-koma-letter)
(require 'org-checklist)

;;; enable org-protocol
(unless (server-running-p)
  (server-start))
(require 'org-protocol)

(setq org-directory "~/org/"
      org-agenda-files '("~/org/todo.org" "~/org/read-later.org" "~/org/kb.org"))

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "TODAY(o)" "|" "DONE(d)")
        (sequence "CANCELLED(c)")))

;;; agenda
(setq org-log-done 'time
      org-agenda-ndays 10
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines 'near
      org-agenda-start-on-weekday nil
      org-agenda-skip-scheduled-if-deadline-is-shown t)

(defun fyi-summarize-captured-url ()
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
        ("n"
         "Add note to kb"
         entry
         (file "~/org/kb.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
         :prepend t :empty-lines 1)
        ("r"
         "Add link to ~/org/read-later.org via org-protocol"
         entry
         (file "~/org/read-later.org")
         "* [[%:link][%:description]] --- (%u)"
         :prepend t :immediate-finish t)
        ("b"
         "Add bookmark via org-protocol"
         entry
         (file "~/org/kb.org")
         "* %:description :bookmark:\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n\n%?\n\n** Website Summary:\n\n%(fyi-summarize-captured-url)\n\n"
         :prepend t :empty-lines 1)
        ("v"
         "Add bookmark via org-protocol but omit summary"
         entry
         (file "~/org/kb.org")
         "* %:description :bookmark:\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n\n%?"
         :prepend t :empty-lines 1)))

;;; setup diary
(setq diary-file (expand-file-name "~/org/diary")
      org-agenda-include-diary t)
(calendar-set-date-style 'iso)

(require-package 'german-holidays)
(setq holiday-other-holidays holiday-german-holidays)

;;; custom agenda views
(setq org-agenda-custom-commands '(("d" "Today"
                                    ((agenda "" ((org-agenda-ndays 1)))
                                     (todo "TODAY")))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode t)
            (set-input-method 'german-prefix)))

;;; Enable yasnippets in org
(add-hook 'org-mode-hook '(lambda ()
                           (org-set-local 'yas/trigger-key [tab])
                           (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))

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

(add-to-list 'org-latex-classes '("lecture-article"
                                  "\\documentclass[11pt,a4paper]{article}
                                  [DEFAULT-PACKAGES] [PACKAGES]
                                  \\usepackage{titling}
                                  \\usepackage{fancyhdr}
                                  \\usepackage{mdframed}
                                  [EXTRA] \\pagestyle{fancy}
                                  \\lhead{\\thetitle}
                                  \\rhead{\\theauthor}
                                  \\renewcommand{\\headrulewidth}{0.0}
                                  \\newenvironment{exam}{\\begin{center}\\begin{mdframed}[backgroundcolor=yellow]}{\\end{mdframed}\\end{center}}"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; misc.
(setq org-export-allow-bind-keywords t  ; allows the use og #+BIND in org files
      org-export-date-timestamp-format "%Y-%m-%d"
      org-footnote-auto-label 'plain    ; generate numbered footnotes like [1]
      )

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

;;; bibdexk/bibtex/citation links
(org-add-link-type
 "x-bdsk"
 (lambda (path)
   (browse-url (concat "x-bdsk:" path)))
 (lambda (path desc format)
   (case format
     ('html
      (format "<font style=\"background-color: red\">Check %s in BibDesk</font>" desc))
     ('latex
      (when (string-match "^cite:\\(.*\\)" desc)
        (format "\\cite{%s}" (match-string 1 desc)))))))

;;; use xelatex with bibtex
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "bibtex %b"
                              "xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;;; use Skim to open PDFs
(add-to-list 'org-file-apps '("\\.pdf\\'" . "/Applications/Skim.app/Contents/SharedSupport/displayline 1 %s"))

;;; org-download
(add-to-list 'load-path (expand-file-name "org-download" emacs-d-vendor))
(require 'org-download)

(defun fyi-configure-org-download ()
  "Configures `org-download'."
  (setq org-download-image-dir "~/org/org-download-images/"
        org-download-heading-lvl nil
        org-download-image-width 200))

(add-hook 'org-mode-hook #'fyi-configure-org-download)

;;; custom easy templates
(add-to-list 'org-structure-template-alist
             '("x" "#+BEGIN_EXAM\n?\n#+END_EXAM" ""))

;;; limit the width
(defun fyi-org-width ()
  (set-fill-column 110)
  (auto-fill-mode t))
(add-hook 'org-mode-hook 'fyi-org-width)

;;; org-mode narrowed navigation
(defun fyi-org-next-heading-narrowed (direction)
  "Go to the next heading and show it narrowed.
If DIRECTION is 1 forward to the next heading. If DIRECTION
is -1 go to the previous heading."
  (widen)
  (org-forward-heading-same-level direction)
  (org-narrow-to-subtree)
  (org-show-subtree)
  (org-cycle-hide-drawers 'all))
(define-key org-mode-map (kbd "C-c j") (lambda ()
                                         (interactive)
                                         (fyi-org-next-heading-narrowed 1)))
(define-key org-mode-map (kbd "C-c k") (lambda ()
                                         (interactive)
                                         (fyi-org-next-heading-narrowed -1)))

;;; org-mode abbrevs)
;; https://stackoverflow.com/questions/18232384/how-to-replace-a-string-with-a-non-backslashed-string-in-emacs-abbrev-mode
(abbrev-table-put org-mode-abbrev-table :regexp "\\(\\\\[a-z0-9@]+\\)")
(define-abbrev org-mode-abbrev-table "\Rightarrow" "⇒")
(define-abbrev org-mode-abbrev-table "\\rightarrow" "→")
(add-hook 'org-mode-hook 'abbrev-mode)

;;; use org-special-* keys
(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-special-ctrl-o t)

;;; org/ispell ignorables
;;; see: http://endlessparentheses.com/ispell-and-org-mode.html
(defun fyi-org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'fyi-org-ispell)


(provide 'init-org-mode)
