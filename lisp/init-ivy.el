(require 'ivy)
(require 'cl-lib)
(require-package 'counsel)

(ivy-mode 1)
(global-set-key (kbd "<f2>") (lambda (arg)
                               (interactive "p")
                               (cond
                                 ((= arg 4)
                                  (counsel-ag))
                                 ((= arg 16)
                                  (search-kb))
                                 (t
                                  (counsel-git-grep)))))
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)

;;; [[http://endlessparentheses.com/visit-directory-inside-a-set-of-directories.html][Visit Directory inside a Set of Directories with Emacs]]
(defcustom fyi-favorite-directories
  '("~/dev/" "~/current/" "~/Documents/Archive/" "~/.emacs.d/" "~/quicklisp/local-projects/"
    "~/Dropbox/Lectures/HS-Mannheim/")
  "List of favorite directories.
Used in `fyi-visit-favorite-dir'. The order here
affects the order that completions will be offered."
  :type '(repeat directory)
  :group 'fyi)

(defun fyi-visit-favorite-dir (files-too)
  "Offer all directories inside a set of directories.
Compile a list of all directories inside each element of
`fyi-favorite-directories', and visit one of them with
`ido-completing-read'.
With prefix argument FILES-TOO also offer to find files."
  (interactive "P")
  (let ((completions
         (mapcar #'abbreviate-file-name
           (cl-remove-if-not
            (if files-too #'file-readable-p
              #'file-directory-p)
            (apply #'append
              (mapcar (lambda (x)
                        (directory-files
                         (expand-file-name x)
                         t "^[^\.].*" t))
                fyi-favorite-directories))))))
    (dired
     (ivy-completing-read "Open directory: "
                          completions 'ignored nil ""))))
(global-set-key (kbd "C-x d") #'fyi-visit-favorite-dir)

(provide 'init-ivy)
