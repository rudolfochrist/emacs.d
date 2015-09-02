;;; init-defuns.el --- utility functions
;;; Code:

(defun fyi-format-json ()
  "Pretty format json."
  (interactive)
  (shell-command-on-region
   (mark) (point) "python -m json.tool" (buffer-name) t))

(defun fyi-format-xml ()
  "Pretty format xml."
  (interactive)
  (shell-command-on-region
   (mark) (point) "xmllint --format -" (buffer-name) t))

(defun fyi-enumerate-list (lst)
  "Transform LST to an enumerated alist.

Example:
'(x y) gets transformed to '((x . 1) (y . 2))"
  (let ((idx 0))
    (cl-mapcar (lambda (elt)
                 (cons elt (cl-incf idx)))
               lst)))

(cl-defun fyi-date-revision (&optional (format-string "%Y%m%d-%3N"))
  "Generate a date-based revision string."
  (format-time-string format-string))

(defun fyi-update-version-string (version-values &optional create-tag-p)
  "Update a version at point.

The version string must have the MAJOR.MINOR.PATCH format.

The VERSION-VALUES has the form
'((\"0\" . 1) (\"12\" . 2) (\"0\" . 3))

where the `car' is the new value and the `cdr' is the positon. The position
is mapped with

MAJOR = 1
MINOR = 2
PATCH = 3

When called interactivley than only the PATH gets updated with the value
provided by `fyi-date-revision'.

When called interactively with a prefix-argument the values must
entered in the minibuffer.

If CREATE-TAG-P is non-nil than the current file gets saved, committed
and a tag is generated."
  (interactive
   (cond
     ((equal current-prefix-arg '(4))
      (let ((major (read-string "Major: "))
            (minor (read-string "Minor: "))
            (patch (read-string "Patch: "))
            (git-tag (y-or-n-p "Create tag? ")))
        (list (fyi-enumerate-list (list major minor patch))
              git-tag)))
     (t
      (list (list (cons (fyi-date-revision) 3))
            nil))))
  (re-search-forward "\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)\\.\\([[:digit:]]+-?[[:digit:]]\\{0,3\\}\\)")
  (when (match-string 0)
    (mapc (lambda (version)
            (replace-match (car version) nil nil nil (cdr version)))
          version-values)
    (when create-tag-p
      (let ((new-version (thing-at-point 'symbol)))
        (save-buffer)
        (magit-stage-item (buffer-file-name))
        (magit-commit-internal "commit"
                               (list "--all" (format "-m v%s" new-version)))
        (magit-tag (format "v%s" new-version) "HEAD")))))



(provide 'init-defuns)

;;; init-defuns.el ends here
