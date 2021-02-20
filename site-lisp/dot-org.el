;;; dot-org.el --- My Org Mode Configuration File

;;; Commentary:
;;
;; Title says it all :]

;;; Code:

;;; setup use-package and org if not already done. That way I can use
;;; just this file for commandline org exports
(unless (featurep 'use-package)
  (let ((package-dir "~/.emacs-packages/"))
    (add-to-list 'load-path (car (directory-files package-dir t "^use-package-")))
    (add-to-list 'load-path (car (directory-files package-dir t "^org-plus-contrib-")))
    (add-to-list 'load-path (car (directory-files package-dir t "^bind-key-")))
    (require 'use-package)
    (require 'package)))

;;; this has to be set before org.el is loaded
(defvar org-export-backends '(ascii html latex))

(unless (package-installed-p 'org)
  (use-package org
    :ensure org-plus-contrib))
(require 'ox)
(require 'ox-md)
(require 'ox-texinfo)
(require 'ox-latex)
(require 'org-tempo) ; brings back the easy-templates

;;; org basics
(setq org-startup-indented t
      org-image-actual-width '(450)
      org-startup-with-inline-images t
      org-directory "~/org/")

;;; enable visual-line-mode
(add-hook 'org-mode-hook 'visual-line-mode)

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

(setq  org-export-allow-bind-keywords t  ; allows the use og #+BIND in org files
       org-export-date-timestamp-format "%Y-%m-%d"
       org-latex-prefer-user-labels t
       org-footnote-auto-label t    ; generate numbered footnotes like [fn:1]
                                        ;org-latex-hyperref-template (toggle-org-latex-hyperref-colorlinks t)
       )


;;; latex compiler settings
(setq org-latex-compiler "xelatex"
      org-latex-pdf-process '("%latex -interaction nonstopmode %f"
                              "%bib %b"
                              "makeindex %b"
                              "PATH=\"/usr/bin:$PATH\" makeglossaries %b"  ; use system perl for makeglossaries
                              "%latex -interaction nonstopmode %f"
                              "%latex -interaction nonstopmode %f"))

;;; limit the width
(defun fyi-org-width ()
  "Limit tthw buffer width fixed to 110."
  (set-fill-column 110)
  (auto-fill-mode t))
(add-hook 'org-mode-hook 'fyi-org-width)

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
  "Create a footnote in latex for each link.

So when printed the information isn't lost.
Argument TEXT is the link's text.
Argument BACKEND is the use export backend.
Argument INFO - I have no idea what this does."
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
  "Open the reference.

PATH is the reference headline."
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
  "Format the texinfo reference."
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
   (shell . t)
   (dot . t)))

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

(provide 'dot-org)

;;; dot-org.el ends here
