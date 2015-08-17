(require-package 'org-page)

(setq op/repository-directory "~/dev/rudolfochrist.github.io/"
      op/site-domain "https://rudolfochrist.github.io"
      op/site-main-title "Sebastian Christ"
      op/site-sub-title "(funcall (λ ()))"
      op/personal-github-link "https://github.com/rudolfochrist"
      op/theme-root-directory (concat op/repository-directory "themes/"))

(add-to-list 'op/category-config-alist
             '("blog" ;; this is the default configuration
               :show-meta t
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/blog/%y/%m/%d/%t/"
               :sort-by :date     ;; how to sort the posts
               :category-index t))

(defun op/do-preview ()
  (interactive)
  (let ((op/repository-org-branch (car (vc-git-branches))))
    (op/do-publication nil nil "~/Desktop/org-page-preview/preview" nil nil)))

(provide 'init-org-page)
