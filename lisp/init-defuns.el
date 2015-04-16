(defun fyi/format-json ()
  (interactive)
  (shell-command-on-region
   (mark) (point) "python -m json.tool" (buffer-name) t))

(defun fyi/format-xml ()
  (interactive)
  (shell-command-on-region
   (mark) (point) "xmllint --format -" (buffer-name) t))

(defun fyi/configure-flyspell (dictionary)
  "Applies flyspell settings"
  (ispell-change-dictionary dictionary)
  (set-input-method "german-prefix")
  (flyspell-mode 1))

(defun fyi/search-kb (search-string)
  (interactive "MSearch String: ")
  (grep (concat "grep --color -inH -C 3 -E "
                (shell-quote-argument search-string)
                " ~/org/kb.org"))
  (other-window 1))

(global-set-key (kbd "C-c C-c p") 'fyi/search-kb)

(defun fyi/enumerate-list (lst)
  "Transform LIST to an enumerated alist.

Example:
'(x y) gets transformed to '((x . 1) (y . 2))"
  (let ((idx 0))
    (cl-mapcar (lambda (elt)
                 (cons elt (cl-incf idx)))
               lst)))

(cl-defun fyi/date-revision (&optional (format-string "%Y%m%d-%3N"))
  "Generate a date-based revision string."
  (format-time-string format-string))

(defun fyi/update-version-string (version-alist &optional create-tag-p)
  (interactive
   (cond
     ((equal current-prefix-arg '(4))
      (let ((major (read-string "Major: "))
            (minor (read-string "Minor: "))
            (patch (read-string "Patch: "))
            (git-tag (y-or-n-p "Create tag?")))
        (list (fyi/enumerate-list (list major minor patch))
              git-tag)))
     (t
      (list (list (cons (fyi/date-revision) 3))
            nil))))
  (re-search-forward "\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)\\.\\([[:digit:]]+-?[[:digit:]]\\{0,3\\}\\)")
  (when (match-string 0)
    (mapc (lambda (version)
            (replace-match (car version) nil nil nil (cdr version)))
          version-alist)
    (when create-tag-p
      (let ((new-version (thing-at-point 'symbol)))
        (save-buffer)
        (magit-stage-item (buffer-file-name))
        (magit-commit-internal "commit"
                               (list "--all" (format "-m v%s" new-version)))
        (magit-tag (format "v%s" new-version) "HEAD")))))

(provide 'init-defuns)
