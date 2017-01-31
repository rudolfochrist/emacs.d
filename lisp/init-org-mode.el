;;; local orgmode
;; (add-to-list 'load-path "~/prj/org-mode/lisp/")
;; (add-to-list 'load-path "~/prj/org-mode/contrib/lisp/")
;; (require 'org)

(require-package 'org-plus-contrib
                 :require 'org)
(require 'ox-latex)
(require 'ox-koma-letter)
(require 'ox-md)
(require 'ox-texinfo)
(require 'org-checklist)

(unless (server-running-p)
  (server-start))
(require 'org-protocol)

;;; Don't use cache to prevent arbitrary crashes
;;; http://mid.gmane.org/m2tw91ij98.fsf%2540gmail.com
(setq org-element-use-cache nil)

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
      org-agenda-files '("~/org/tasks/personal.org"
                         "~/org/tasks/m-creations.org")
      org-web-capture-file "~/org/web.org"
      org-kb-file "~/org/kb.org")

(global-set-key
 (kbd "C-x t j")
 (defhydra hydra-prominent-files (:color blue
                                         :hint nil)
   "
Quickly jump to files:
_b_ooks
_f_inances
_k_nowledge base
_B_ug Knowledge Base
_a_nnual book expenses
"
   ("b" (find-file "~/org/books.org"))
   ("B" (find-file "~/org/bug-kb.org"))
   ("f" (find-file (format "~/archive/Finances/%s/%s.ledger"
                           (nth 2 (calendar-current-date))
                           (nth 2 (calendar-current-date)))))
   ("k" (find-file "~/org/kb.org"))
   ("a" (find-file (format "~/archive/Finances/%s/book-expenses.org"
                           (nth 2 (calendar-current-date)))))))

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "PROJECT(p)" "WAITING(w)" "DELEGATED(k)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELED(c)"))
      org-stuck-projects '("TODO=\"PROJECT\"" ("TODO" "WAITING" "DELEGATED") nil "")
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
      org-agenda-start-on-weekday nil   ; start agenda on current day
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'near
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-deadline-warning-days 7)

;;; org-capture
(global-set-key (kbd "<M-f12>") #'org-smart-capture)
(setq org-gnus-prefer-web-links t)
(setq org-capture-templates
      '(("a" "Add task" entry
         (file+headline "~/org/tasks/personal.org" "Inbox")
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
        ("b" "Add bookmark" entry
         (file "~/org/kb.org")
         "* %?  :bookmark:
:PROPERTIES:
:CREATED: %U
:URL:
:END:
"
         :prepend t :empty-lines 1)
        ("w" "Capture website" entry
         (file "~/org/web.org")
         "* %:description :bookmark:
:PROPERTIES:
:CREATED: %U
:URL: %l
:END:"
         :prepend t :empty-lines 1 :immediate-finish t)))

;;; refile
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-arefile-targets
      '((org-agenda-files . (:level . 1))
        (org-agenda-files . (:todo . "PROJECT"))
        (("~/org/kb.org") . (:maxlevel . 1))))

(defun fyi-projectize-new-refile-targets (parent child)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char
       (org-find-exact-headline-in-buffer child (current-buffer) t))
      (org-todo "PROJECT"))))

(advice-add 'org-refile-new-child :after #'fyi-projectize-new-refile-targets)

(defun org-refile-web-capture ()
  "Refile link from `org-web-capture-file' to `org-kb-file'."
  (interactive)
  (let ((kb (find-file-noselect org-kb-file))
        (entry (org-element-at-point)))
    (unless (eql 'headline (org-element-type entry))
      (user-error "Must be on a headline!"))
    (org-copy-subtree nil t)
    (with-current-buffer kb
      (goto-char (point-min))
      (outline-next-visible-heading 1)
      (org-yank)
      (save-buffer))
    (message "Refiled %s to KB."
             (org-element-property :title entry))))

;;; setup diary
(setq diary-file (expand-file-name "~/org/diary")
      org-agenda-include-diary t)
(calendar-set-date-style 'iso)

(require-package 'german-holidays)
(setq calendar-holidays holiday-german-holidays)

;;; custom agenda views
(setq org-agenda-custom-commands
      '(("i" "Inbox" tags "CATEGORY=\"Inbox\"&LEVEL=2&TODO<>{DONE\\|CANCELED}"
         ((org-agenda-overriding-header "Inbox:")))
        ("p" "Personal Agenda" agenda ""
         ((org-agenda-category-filter-preset '("-Inbox" "-Work"))))
        ("w" "Work Agenda" agenda ""
         ((org-agenda-category-filter-preset '("+Work"))))
        ("f" "Follow up" tags "fu&email&TODO<>{DONE\\|CANCELED}"
         ((org-agenda-overriding-header "Follow up:")))
        ("A" "All TODOs" tags "TODO=\"TODO\"&CATEGORY<>\"Inbox\""
         ((org-agenda-overriding-header "All TODOs"))) 
        ("W" "Waiting/Delegated tasks" tags "TODO=\"WAITING\"|TODO=\"DELEGATED\""
         ((org-agenda-overriding-header "Waiting/Delegated tasks:")))
        ("P" "Active projects" tags "TODO=\"PROJECT\""
         ((org-agenda-overriding-header "Active Projects:")))
        ("Y" "Someday/Maybe tasks" todo "SOMEDAY"
         ((org-agenda-overriding-header "Someday/Maybe tasks:")))
        ("R" "Review Done/Canceled tasks" tags "TODO={DONE\\|CANCELED}"
         ((org-agenda-overriding-header "Review Done/Canceled tasks:")))
        ("U" "Unscheduled tasks" tags "TODO<>\"\"&TODO<>{DONE\\|SOMEDAY\\|PROJECT\\|CANCELED}"
         ((org-agenda-overriding-header "Unscheduled tasks:")
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'timestamp))))))

;;; org keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<f12>") 'org-agenda)

(defun fyi-org-keybindings ()
  (define-key org-mode-map (kbd "C-c C-r") #'org-refile-web-capture))
(add-hook 'org-mode-hook #'fyi-org-keybindings)

(add-hook 'org-mode-hook 'visual-line-mode)

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
      '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "glg" "glo" "gls" "idx" "ist"
        "log" "nav" "out" "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "bbl" "ilg" "ind"
        "lof" "lot" "lol" "xwm"))

;;; misc.
(defun toggle-org-latex-hyperref-colorlinks (&optional force-colorlinks)
  "Toggel colorlinks=true in LaTeX hyperref setup.

This is great for printing the document in grayscale.

With prefix argumnet or if FORCE-COLORLINKS is non-nil set
hypersetup to include colorlinks=true."
  (interactive "P")
  (let ((prefix "\\hypersetup{\n pdftitle={%t},\n pdfcreator={%c}, \n pdflang={%L},\n colorlinks=")
        (suffix "}\n")
        (colorlinksp (string-match "colorlinks=true" org-latex-hyperref-template)))
    (setq org-latex-hyperref-template
          (concat prefix
                  (if (or force-colorlinks
                          (not colorlinksp))
                      "true"
                    "false")
                  suffix))))

(setq org-export-allow-bind-keywords t  ; allows the use og #+BIND in org files
      org-export-date-timestamp-format "%Y-%m-%d"
      org-latex-prefer-user-labels t
      org-footnote-auto-label t    ; generate numbered footnotes like [fn:1]
      org-latex-hyperref-template (toggle-org-latex-hyperref-colorlinks 'force))

(add-to-list 'org-latex-packages-alist '("" "color" nil))


(setq org-startup-indented t)           ; Use clean view

;;; display inline images on startup
(setq org-image-actual-width '(450))
(setq org-startup-with-inline-images t)

;;; bibdexk/bibtex/citation links
(defun org-bdsk-open (path)
  (browse-url (concat "x-bdsk:" path)))

(defun org-bdsk-export (path description export-backend)
  (let ((cite-key (subseq path 2)))
    (case export-backend
      (html
       (format "<p>[%s] %s" cite-key description))
      (latex
       (format "\\par [%s] \\textbf{%s}" cite-key description)))))

(org-link-set-parameters "x-bdsk"
                         :follow 'org-bdsk-open
                         :export 'org-bdsk-export)

;;; message links
(defun org-message-store-link ()
  (when (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (let* ((group (car gnus-article-current))
           (article (cdr gnus-article-current))
           (data (gnus-data-find-list article))
           (header (gnus-data-header (car data)))
           (message-id (mail-header-message-id header))
           (raw-subject (mail-header-subject header))
           (subject (and raw-subject (rfc2047-decode-string raw-subject))))
      (org-store-link-props
       :type "message"
       :link (format "message://%s/%s"
                     (base64-encode-string group)
                     (substring message-id 1 -1))
       :description (subst-char-in-string
                     ?\[ ?\{ (subst-char-in-string
                              ?\] ?\} subject))))))

(defun org-message-open (path)
  (cl-destructuring-bind (group message-id)
      (split-string (substring path 2) "/")
    (gnus-goto-article (base64-decode-string group)
                       message-id)))

(org-link-set-parameters "message"
                         :store 'org-message-store-link
                         :follow 'org-message-open)

;;; use xelatex with bibtex
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "bibtex %b"
                              "makeindex %b"
                              "PATH=\"/usr/bin:$PATH\" makeglossaries %b"  ; use system perl for makeglossaries
                              "xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))

;;; org-download
(require-package 'org-download)
(setq org-download-image-dir "~/org/org-download-images/"
      org-download-heading-lvl nil
      org-download-image-width 200)

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

;;; texinfo export

;;; use strike-through to markup vars in texinfo
(add-to-list 'org-texinfo-text-markup-alist '(strike-through . "@var{%s}"))

(defun org-texinfo-ref-open (path)
  (let ((headline (org-find-exact-headline-in-buffer path (current-buffer) t)))
    (if headline
        (goto-char headline)
      ;; try to find anchor
      (let ((anchor (save-excursion
                      (save-restriction
                        (widen)
                        (goto-char (point-min))
                        (search-forward (format "@anchor{%s}" path) nil t)
                        (when (match-string 0)
                          (point))))))
        (when anchor
          (goto-char anchor))))))

(defun org-texinfo-reference-export (ref-type path description backend)
  (when (eql backend 'texinfo)
    (format "@%s{%s}"
            ref-type
            (if description
                (format "%s,,%s" path description)
              path))))

(defun org-texinfo-xref-export (path desc backend)
  (org-texinfo-reference-export "xref" path desc backend))

(org-link-set-parameters "texixref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-xref-export)

(defun org-texinfo-ref-export (path desc backend)
  (org-texinfo-reference-export "ref" path desc backend))

(org-link-set-parameters "texiref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-ref-export)

(defun org-texinfo-pxref-export (path desc backend)
  (org-texinfo-reference-export "pxref" path desc backend))

(org-link-set-parameters "texipxref"
                         :follow 'org-texinfo-ref-open
                         :export 'org-texinfo-pxref-export)

(provide 'init-org-mode)
