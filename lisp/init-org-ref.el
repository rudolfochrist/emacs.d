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
      org-ref-pdf-directory "~/Dropbox/Papers/Library/")

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-c fr") #'org-ref-helm-insert-ref-link)
  (define-key org-mode-map (kbd "C-c fl") #'org-ref-helm-insert-label-link))

(provide 'init-org-ref)
