;;; org-export backends
;;; apperently this has to be set BEFORE org.el is loaded!
(setq org-export-backends '(ascii html latex texinfo))

;; (add-to-list 'load-path "~/dev/org-mode/lisp/")
;; (add-to-list 'load-path "~/dev/org-mode/contrib/lisp/")
;; (require 'org)

(require-package 'org-plus-contrib
                 :require 'org)
(require 'ox-latex)
(require 'ox-koma-letter)
(require 'org-checklist)

;;; use some extras
(require 'ox-extra)
(ox-extras-activate '(latex-header-blocks ignore-headlines))

;;; jwiegley's smart capture
(require-package 'org-smart-capture :from-dir emacs-d-site-lisp)

;;; overwrite because of bugs
(defun org-latex-header-blocks-filter (backend)
  (when (org-export-derived-backend-p backend 'latex)
    (let ((blocks
	   (org-element-map (org-element-parse-buffer 'greater-element nil) 'export-block
	     (lambda (block)
	       (when (and (string= (org-element-property :type block) "LATEX")
			  (string= (org-export-read-attribute
				    :header block :header)
				   "yes"))
		 (list (org-element-property :begin block)
		       (org-element-property :end block)
		       (org-element-property :value block)))))))
      (mapc (lambda (block)
	      (goto-char (nth 0 block))
	      (let ((contents-lines (split-string (nth 2 block) "\n" t)))
                (delete-region (nth 0 block) (nth 1 block))
                (dolist (line contents-lines)
                  (insert (concat "#+latex_header: "
                                  (replace-regexp-in-string "\\` *" "" line)
                                  "\n")))))
	    ;; go in reverse, to avoid wrecking the numeric blocks
	    ;; earlier in the file
	    (reverse blocks)))))

;;; org files
(setq org-directory "~/org/"
      org-agenda-files '("~/org/tasks/todo.org"))

(global-set-key
 (kbd "C-x t j")
 (defhydra hydra-prominent-files (:color blue
                                         :hint nil)
   "
Quickly jump to files:
_t_odos
_b_ooks
_f_inances
_k_nowledge base
_r_ead later
_a_nnual book expenses
"
   ("t" (find-file "~/org/tasks/todo.org"))
   ("b" (find-file "~/org/books.org"))
   ("f" (find-file "~/org/finances.ledger"))
   ("k" (find-file "~/org/kb.org"))
   ("r" (find-file "~/org/read-later.org"))
   ("a" (find-file (format "~/Documents/Archive/Finances/%s/book-expenses.org"
                           (nth 2 (calendar-current-date)))))))

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJECT(p)" "WAITING(w)" "DELEGATED(k)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)"))
      org-todo-keyword-faces
      '(("PROJECT" :weight bold :foreground "dark magenta")
        ("DONE" :weight bold :foreground "forest green")
        ("CANCELED" :wight bold :foreground "forest green")
        ("SOMEDAY" :weight bold :foreground "dark goldenrod")
        ("TODO" :weight bold :foreground "medium blue")
        ("APPT" :weight bold :foreground "medium blue")
        ("WAITING" :weight bold :foreground "red")
        ("DELEGATED" :weight bold :foreground "red")))

;;; archiving DONE tasks
;;; https://github.com/jwiegley/dot-emacs/blob/master/dot-org.el#L330
(defvar org-archive-expiry-days 9
  "The number of days after which a completed task should be auto-archived.
This can be zero for immediate or a floating point value.")

(defun org-archive-expired-tasks ()
  "Archive task with a completion date after `org-archive-expiry-days'."
  (interactive)
  (flet ((prop (property element)
               (org-element-property property element))
         (completep (headline)
                    (member (prop :todo-type headline)
                            '(done canceled)))
         (expirep (headline)
                  (>= (time-to-number-of-days
                       (time-subtract (current-time)
                                      (org-time-string-to-time
                                       (prop :raw-value (prop :closed headline)))))
                      org-archive-expiry-days))
         (level-2-p (headline)
                    (= 2 (prop :level headline))))
    (save-excursion
      (goto-char (point-min))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (headline)
          (when (and (level-2-p headline)
                     (completep headline)
                     (expirep headline))
            (goto-char (prop :begin headline))
            (org-archive-subtree)))))))

;;; agenda
(setq org-log-done 'time
      org-agenda-span 'week
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled t
      org-agenda-todo-ignore-deadlines 'near
      org-agenda-start-on-weekday nil
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-deadline-warning-days 7)

(defun fyi-summarize-captured-url ()
  "Summarizes the website for the given URL using sumy (https://github.com/miso-belica/sumy)."
  (let ((url (fyi-get-current-url)))
    (with-temp-buffer
      (call-process "sumy" nil t nil "text-rank" (format "--url=%s" url) "--length=10%")
      (buffer-string))))

;;; org-capture
(global-set-key (kbd "<M-f12>") #'org-smart-capture)
(setq org-gnus-prefer-web-links t)
(setq org-capture-templates
      '(("a" "Add task" entry
         (file+headline "~/org/tasks/todo.org" "Inbox")
         "* TODO %?
SCHEDULED: %t
ADDED: %U"
         :prepend t)
        ("n" "Add note to kb" entry
         (file "~/org/kb.org")
         "* %?
:PROPERTIES:
:CREATED: %U
:END:\n\n"
         :prepend t :empty-lines 1)
        ("r" "Add link to ~/org/read-later.org" entry
         (file "~/org/read-later.org")
         "* %(fyi-get-current-title) --- (%u)
:PROPERTIES:
:URL: %(fyi-get-current-url)
:END:\n\n"
         :prepend t :immediate-finish t)
        ("y" "Add link to ~/org/read-later.org" entry
         (file "~/org/read-later.org")
         "* %:description  --- (%u)
:PROPERTIES:
:URL: %:link
:END:\n\n"
         :prepend t :immediate-finish t)
        ("b" "Add bookmark" entry
         (file "~/org/kb.org")
         "* %(fyi-get-current-title) :bookmark:
:PROPERTIES:
:CREATED: %U
:URL: %(fyi-get-current-url)
:END:

%?

** Website Summary:

%(fyi-summarize-captured-url)\n\n"
         :prepend t :empty-lines 1)
        ("v" "Add bookmark but omit summary" entry
         (file "~/org/kb.org")
         "* %(fyi-get-current-title) :bookmark:
:PROPERTIES:
:CREATED: %U
:URL: %(fyi-get-current-url)
:END:

%?"
         :prepend t :empty-lines 1)))

;;; refile
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets
      '(("~/org/tasks/todo.org" :level . 1)
        (org-agenda-files :todo . "PROJECT")))

;;; setup diary
(setq diary-file (expand-file-name "~/org/diary")
      org-agenda-include-diary t)
(calendar-set-date-style 'iso)

(require-package 'german-holidays)
(setq calendar-holidays holiday-german-holidays)

;;; custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Week Agenda" agenda ""
         ((org-agenda-category-filter-preset '("-INBOX"))))
        ("D" "Day Agenda" agenda ""
         ((org-agenda-category-filter-preset '("-INBOX"))
          (org-agenda-span 'day)))
        ("p" "Active projects" tags "TODO=\"PROJECT\""
         ((org-agenda-overriding-header "Active Projects:")))
        ("u" "Uncategorized items" tags "CATEGORY=\"INBOX\"&LEVEL=2"
         ((org-agenda-overriding-header "Uncategorized items:")))
        ("j" "All TODOs" tags "TODO=\"TODO\"&CATEGORY<>\"INBOX\""
         ((org-agenda-overriding-header "All TODOs")))
        ("w" "Waiting/Delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
         ((org-agenda-overriding-header "Waiting/Delegated tasks:")))
        ("y" "Someday/Maybe tasks" todo "SOMEDAY"
         ((org-agenda-overriding-header "Someday/Maybe tasks:")))
        ("x" "Review Done/Canceled tasks" tags "TODO={DONE\\|CANCELED}&CATEGORY<>\"INBOX\"&LEVEL=2"
         ((org-agenda-overriding-header "Review Done/Canceled tasks:")))
        ("Y" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|SOMEDAY\\|PROJECT\\|CANCELED}"
         ((org-agenda-overriding-header "Unscheduled tasks:")
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'timestamp))))))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)

(add-hook 'org-mode-hook 'visual-line-mode)

;;; Enable yasnippets in org
(add-hook 'org-mode-hook '(lambda ()
                            (org-set-local 'yas/trigger-key [tab])
                            (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))

;;; latex export customization
(add-to-list 'org-latex-classes
             '("fyi-article"
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

(add-to-list 'org-latex-classes
             '("lecture-article"
               "\\documentclass[11pt,a4paper]{article}
                                  [DEFAULT-PACKAGES] [PACKAGES]
                                  \\usepackage{titling}
                                  \\usepackage{fancyhdr}
                                  \\usepackage{mdframed}
                                  \\usepackage{tikz}
                                  \\usepackage{venndiagram}
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

(add-to-list 'org-latex-classes
             '("scrbook"
               "\\documentclass[12pt,a4paper]{scrbook}
               [no-default-packages]
               [no-packages]
               [extra]"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; extend latex "log" files
(setq org-latex-logfiles-extensions
      '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out"
        "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "bbl" "ilg" "ind" "lof" "lot"))

;;; misc.
(setq org-export-allow-bind-keywords t  ; allows the use og #+BIND in org files
      org-export-date-timestamp-format "%Y-%m-%d"
      org-footnote-auto-label 'plain    ; generate numbered footnotes like [1]
      org-latex-hyperref-template "\\hypersetup{\n pdfauthor={%a},\n pdftitle={%t},\n pdfkeywords={%k},\n pdfsubject={%d},\n pdfcreator={%c}, \n pdflang={%L}, \n colorlinks=true}\n")

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

;;; message links
(org-add-link-type
 "message"
 (lambda (ref)
   (let* ((split (split-string (substring-no-properties ref 2)
                               "/"))
          (group (base64-decode-string (car split)))
          (message-id (cadr split)))
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (gnus-goto-article group message-id))))
 ;; no export
 nil)

(defun org-get-message-link (&optional title)
  (with-current-buffer gnus-original-article-buffer
    (let ((message-id (gnus-fetch-field "message-id"))
          (subject (or title (gnus-fetch-field "subject"))))
      (org-make-link-string (format "message:%s" message-id)
                            (rfc2047-decode-string subject)))))

(defun org-insert-message-link (&optional arg)
  (interactive "P")
  (insert (org-get-message-link (if arg "writes"))))

(defun fyi-get-current-url ()
  "Fetches the active tab's URL from Google Chrome."
  (do-applescript
   "tell application \"Google Chrome\"
URL of active tab of front window
end tell"))

(defun fyi-get-current-title ()
  "Fetches the active tab's title from Google Chrome"
  (do-applescript
   "tell application \"Google Chrome\"
title of active tab of front window
end tell"))

(defun org-get-url-link ()
  (org-make-link-string
   (fyi-get-current-url)
   (fyi-get-current-title)))

(defun org-insert-url-link ()
  (interactive)
  (insert (org-get-url-link)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-x l") #'org-insert-message-link)
            (local-set-key (kbd "C-c C-x u") #'org-insert-url-link)))

;;; use xelatex with bibtex
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "bibtex %b"
                              "makeindex %b"
                              "xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;;; use Skim to open PDFs
(add-to-list 'org-file-apps '("\\.pdf\\'" . "/Applications/Skim.app/Contents/SharedSupport/displayline 1 %s"))

;;; org-download
(require-package 'org-download
                 :from-dir emacs-d-site-lisp)

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

;;; org-mode abbrevs)
;; https://stackoverflow.com/questions/18232384/how-to-replace-a-string-with-a-non-backslashed-string-in-emacs-abbrev-mode
(abbrev-table-put org-mode-abbrev-table :regexp "\\(\\\\[a-z0-9@]+\\)")
(define-abbrev org-mode-abbrev-table "\\Rightarrow" "⇒")
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

;;; adjust ispell dictionary based on export language
(defun fyi-org-adjust-ispell ()
  (let ((language (plist-get (org-export-get-environment) :language)))
    (ispell-change-dictionary (if (string-equal language "de")
                                  "german8"
                                "en_US"))))
(add-hook 'org-mode-hook 'fyi-org-adjust-ispell)

;;; export and move
(defvar org-export-directory nil
  "Override this file-locally.")

(defun org-export-html-and-move ()
  "Exports org buffer and moves the file to `org-export-directory'."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (user-error "Please go to an Org Mode buffer."))
  (org-html-export-to-html)
  (when (and org-export-directory
             (file-exists-p org-export-directory))
    (let ((file-name (format "%s.html" (file-name-base (buffer-file-name))))
          (cwd (file-name-directory (buffer-file-name))))
      (rename-file (concat cwd file-name)
                   (concat org-export-directory file-name)
                   t))))

;;; org export filters
(defvar org-export-latex-add-link-footnotes nil
  "If non-nil links will be added as footnotes if exported to latex.")

(defun org-export-latex-link-footnote (text backend info)
  "Create a footnote in latex for each link. So when printed the information isn't lost."
  (when (and org-export-latex-add-link-footnotes
             (org-export-derived-backend-p backend 'latex)
             (string-match "\\\\href{\\(.*\\)}{\\(.*\\)}" text))
    (when (some (lambda (type)
                  (string-prefix-p type (match-string 1 text)))
                '("http" "https" "ftp" "mailto" "doi"))
      (format "%s \\footnote{\\url{%s}} " text (match-string 1 text)))))
(add-to-list 'org-export-filter-link-functions #'org-export-latex-link-footnote)

(provide 'init-org-mode)
