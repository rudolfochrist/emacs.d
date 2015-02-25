(require 'ido)
(require-package 'ido-vertical-mode)
(require 'cl-lib)


(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)

;;; [[http://endlessparentheses.com/visit-directory-inside-a-set-of-directories.html][Visit Directory inside a Set of Directories with Emacs]]
(defcustom fyi/favorite-directories
  '("~/dev/" "~/org/" "~/Documents/Archive/" "~/.emacs.d/lisp/")
  "List of favorite directories.
Used in `fyi/visit-favorite-dir'. The order here
affects the order that completions will be offered."
  :type '(repeat directory)
  :group 'fyi)

(defun fyi/visit-favorite-dir (files-too)
  "Offer all directories inside a set of directories.
Compile a list of all directories inside each element of
`fyi/favorite-directories', and visit one of them with
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
                fyi/favorite-directories))))))
    (dired
     (ido-completing-read "Open directory: "
                          completions 'ignored nil ""))))
(global-set-key (kbd "C-x d") #'fyi/visit-favorite-dir)

(provide 'init-ido)
