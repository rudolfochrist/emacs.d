(require-package 'org-page)

(setq op/repository-directory "~/dev/rudolfochrist.github.io/"
      op/site-domain "https://rudolfochrist.github.io"
      op/site-main-title "Sebastian Christ"
      op/site-sub-title "(funcall (λ ()))"
      op/personal-github-link "https://github.com/rudolfochrist")

(add-to-list 'op/category-config-alist
             '("blog" ;; this is the default configuration
               :show-meta t
               :show-comment nil
               :uri-generator op/generate-uri
               :uri-template "/blog/%y/%m/%d/%t/"
               :sort-by :date     ;; how to sort the posts
               :category-index t))

(provide 'init-org-page)