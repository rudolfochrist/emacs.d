(require-package 'org-ref)
(dolist (pkg '(org-ref-url-utils
               org-ref-pdf
               org-ref-bibtex
               org-ref-arxiv
               org-ref-isbn
               org-ref-helm-bibtex
               doi-utils))
  (require pkg))

(setq org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Dropbox/Papers/Library/"
      org-ref-default-citation-link "citep")

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c ff") #'org-ref-helm-insert-cite-link)
  (define-key org-mode-map (kbd "C-c fr") #'org-ref-helm-insert-ref-link)
  (define-key org-mode-map (kbd "C-c fl") #'org-ref-helm-insert-label-link)
  (define-key org-mode-map (kbd "C-c fg") #'org-ref-insert-glossary-link))

(defun fyi-get-pdf-filename (key)
  "Resolves the pdf path for key"
  (format "%s0-%s.pdf"
          (file-name-as-directory org-ref-pdf-directory)
          key))
(setq org-ref-get-pdf-filename-function #'fyi-get-pdf-filename)

(defun fyi-helm-bibtex-open (path)
  (error "%s" path))

(require 'bibtex-completion)

;;; Patching this because my PDF file names are different
(defun bibtex-completion-find-pdf-in-library (key-or-entry)
  "Searches the directories in `bibtex-completion-library-path' for a
PDF whose names is composed of the BibTeX key plus \".pdf\".  The
path of the first matching PDF is returned."
  (let* ((key (if (stringp key-or-entry)
                  key-or-entry
                (bibtex-completion-get-value "=key=" key-or-entry)))
         (path (-first 'f-file?
                       (list (fyi-get-pdf-filename key)))))
    (when path (list path))))

(setq bibtex-completion-library-path org-ref-pdf-directory
      bibtex-completion-pdf-open-function #'org-open-file)

(provide 'init-org-ref)
