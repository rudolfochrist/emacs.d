(require 'org)
(require 'org-agenda)
(require 'ox-koma-letter)
(require 'ox-md)
(require 'ox-texinfo)
(require 'org-checklist)
(require 'org-protocol)

;;; Don't use cache to prevent arbitrary crashes
;;; http://mid.gmane.org/m2tw91ij98.fsf%2540gmail.com
(setq org-element-use-cache nil)

;;; use some extras
(require 'ox-extra)
(ox-extras-activate '(latex-header-blocks ignore-headlines))

;;; jwiegley's smart capture
(require 'org-smart-capture)

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
      org-agenda-files '("~/org/tasks/inbox.org"
                         "~/org/tasks/projects.org"
                         "~/org/tasks/tickler.org")
      org-web-capture-file "~/org/web.org"
      org-kb-file "~/org/kb.org")

;;; todos setup
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-stuck-projects '("TODO=\"PROJECT\"" ("TODO" "WAITING" "DELEGATED") nil ""))

;;; org-capture
(setq org-gnus-prefer-web-links t
      org-capture-templates
      '(("a" "Add task" entry
         (file+headline "~/org/tasks/inbox.org" "Inbox")
         "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:")
        ("t" "Add tickler" entry
         (file+headline "~/org/tasks/tickler.org" "Tickler File")
         "* %?
SCHEDULED: %^t
:PROPERTIES:
:CREATED: %U 
:END:")
        ("n" "Add note to kb" entry
         (file "~/org/kb.org")
         "* %?
:PROPERTIES:
:CREATED: %U
:END:\n\n"
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
      org-refile-targets '((("~/org/tasks/tickler.org" "~/org/tasks/someday.org")  :level . 1)
                           ("~/org/tasks/projects.org" :maxlevel . 2)))

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

(use-package german-holidays
  :load-path "site-lisp/german-holidays"
  :demand t
  :config (setq calendar-holidays holiday-german-holidays))

;;; agenda
(setq org-log-done 'time                ; create a time stamp when task is DONE!
      org-log-repeat nil                ; don't log time stamps for repeated items
      org-log-into-drawer t             ; this is a lot cleaner
      org-agenda-span 'week
      org-agenda-start-on-weekday nil   ; start agenda on current day
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-todo-ignore-deadlines 'near
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-deadline-warning-days 7)

;;; https://github.com/NicolasPetton/emacs.d/blob/master/hosts/blueberry/init-org.el#L135
(defun org-agenda-select-next-action ()
  (let (should-skip-entry)
    (unless (org-current-todo-p)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (or (org-current-todo-p)
                  ;; if we find a task that is waiting we can't
                  ;; actually proceed to the next action, because
                  ;; we've to wait for the outcome.
                  (org-current-waiting-p))
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-todo-p ()
  (string= "TODO" (org-get-todo-state)))

(defun org-current-waiting-p ()
  (string= "WAITING" (org-get-todo-state)))

(setq org-agenda-custom-commands
      '(("z" "Today"
         ((agenda "" ((org-agenda-span 'day)))
          (alltodo
           ""
           ((org-agenda-files '("~/org/tasks/projects.org"))
            (org-agenda-overriding-header "Next Actions:")
            (org-agenda-skip-function #'org-agenda-select-next-action)))))
        ("b" "Burning"
         ((agenda "" ((org-agenda-entry-types '(:deadline))
                      (org-deadline-warning-days 0)
                      (org-agenda-overriding-header "Burning!")
                      (org-agenda-span 14)))))
        ("i" "Show inbox"
         ((alltodo
           ""
           ((org-agenda-files '("~/org/tasks/inbox.org"))
            (org-agenda-overriding-header "Inbox:")))))
        ("n" "Show next actions"
         ((alltodo
           ""
           ((org-agenda-files '("~/org/tasks/projects.org"))
            (org-agenda-overriding-header "Next Actions:")
            (org-agenda-skip-function #'org-agenda-select-next-action)))))
        ("w" "Show delegated tasks" todo "WAITING"
         ((org-agenda-overriding-header "Waiting for:")))
        ("f" "Show follow ups" tags-todo "fu"
         ((org-agenda-overriding-header "Follow up:")))
        ("@" . "Contexts")
        ("@h" "@home"
         ((tags-todo "@home"
                     ((org-agenda-overriding-header "@home:")
                      (org-agenda-skip-function #'org-agenda-select-next-action)))))
        ("@o" "@office"
         ((tags-todo "@office"
                     ((org-agenda-overriding-header "@office:")
                      (org-agenda-skip-function #'org-agenda-select-next-action)))))
        ("@e" "@email"
         ((tags-todo "@email"
                     ((org-agenda-overriding-header "@email:")
                      (org-agenda-skip-function #'org-agenda-select-next-action)))))
        ("@r" "@errands"
         ((tags-todo "@errands"
                     ((org-agenda-overriding-header "@errands:")
                      (org-agenda-skip-function #'org-agenda-select-next-action)))))
        ("@a" "@anywhere"
         ((tags-todo "@anywhere"
                     ((org-agenda-overriding-header "@anywhere:")
                      (org-agenda-skip-function #'org-agenda-select-next-action)))))))

;;; enable visual-line-mode
(add-hook 'org-mode-hook 'visual-line-mode)

;;; latex export customization
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


;;; latex compiler settings
(setq org-latex-compiler "xelatex"
      org-latex-pdf-process '("%latex -interaction nonstopmode %f"
                              "%bib %b"
                              "makeindex %b"
                              "PATH=\"/usr/bin:$PATH\" makeglossaries %b"  ; use system perl for makeglossaries
                              "%latex -interaction nonstopmode %f"
                              "%latex -interaction nonstopmode %f"))

;;; org buffer niceties.
(setq org-startup-indented t
      org-image-actual-width '(450)
      org-startup-with-inline-images t)

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
(defun org-message-open (path)
  (cl-destructuring-bind (group message-id)
      (split-string (substring path 2) "/")
    (gnus-goto-article (base64-decode-string group)
                       message-id)))

(org-link-set-parameters "message"
                         :follow 'org-message-open)

;;; org-download
(use-package org-download
  :load-path "site-lisp/org-download"
  :demand t
  :init
  (setq org-download-image-dir "~/org/org-download-images/"
        org-download-heading-lvl nil
        org-download-image-width 200))

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

;;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (js . t)
   (lisp . t)
   (latex . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t        
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)   

;;; org-ref

(use-package org-ref
  :load-path "site-lisp/org-ref"
  :demand t
  :bind ((:map
          org-mode-map
          ("C-c f" . org-ref-hydra)))
  :preface
  (defun fyi-get-pdf-filename (key)
    "Resolves the pdf path for key"
    (format "%s0-%s.pdf"
            (file-name-as-directory org-ref-pdf-directory)
            key))
  (defhydra org-ref-hydra (:color teal)
    "org-ref"
    ("f" org-ref-helm-insert-cite-link "add citation link")
    ("r" org-ref-helm-insert-ref-link "add ref link")
    ("l" org-ref-helm-insert-label-link "add label link")
    ("g" org-ref-insert-glossary-link "add glossary link"))
  :init
  (use-package reftex)
  (use-package parsebib
    :load-path "site-lisp/parsebib")
  (use-package biblio
    :load-path "site-lisp/biblio")
  (use-package helm
    :load-path "site-lisp/helm"
    :config
    (use-package helm-bibtex
      :load-path "site-lisp/helm-bibtex"))
  
  (setq org-ref-default-bibliography reftex-default-bibliography
        org-ref-pdf-directory "~/Dropbox/Papers/Library/"
        org-ref-default-citation-link "citep"
        org-ref-get-pdf-filename-function #'fyi-get-pdf-filename)
  :config
  ;; load the rest
  (dolist (util '(org-ref-utils
                  org-ref-latex
                  org-ref-bibtex
                  org-ref-helm-bibtex
                  bibtex-completion))
    (require util)))

;;; code block wrapping
(defun wrap-code-block ()
  (interactive)
  (if (not (region-active-p))
      (user-error "No region selected.")
    (let ((beg (region-beginning))
          (end (region-end)))
      (goto-char end)
      (insert "#+END_SRC\n")
      (goto-char beg)
      (insert "#+BEGIN_SRC "))))

(bind-key "C-M-<" #'wrap-code-block org-mode-map)

;;; org-clock
(require 'org-clock)
(setq org-clock-idle-time 10
      org-clock-persist t
      org-clock-out-remove-zero-time-clocks t
      org-clock-in-resume t)
(org-clock-persistence-insinuate)

(provide 'dot-org)
