(require-package 'org-ref)
(dolist (pkg '(org-ref-url-utils
               org-ref-pdf
               org-ref-bibtex
               org-ref-arxiv
               org-ref-isbn
               org-ref-ivy-bibtex))
  (require pkg))

(setq org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory "~/Dropbox/Papers/Library/"
      org-ref-completion-library 'org-ref-ivy-bibtex)

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c ff") #'org-ref-ivy-insert-cite-link)
  (define-key org-mode-map (kbd "C-c fr") #'org-ref-helm-insert-ref-link)
  (define-key org-mode-map (kbd "C-c fl") #'org-ref-ivy-insert-label-link))

(provide 'init-org-ref)
