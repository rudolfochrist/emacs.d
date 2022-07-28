;;; company-tailwindcss.el --- TailwindCSS Company Backend


;;; Commentary:
;; Completion of TailwindCSS classes.

(require 'company)
(require 'f)
(require 'cl-lib)


;;; Code:

(defvar company-tailwindcss-classes
  (split-string (f-read (f-expand "site-lisp/tailwindcss-classes.txt" user-emacs-directory))))

(defvar company-tailwindcss-major-modes
  '(rjsx-mode web-mode html-mode css-mode typescript-mode))

(defun company-tailwindcss-backend (command &optional arg &rest ignored)
  "Company backend function for TailwindCSS completion.
Argument COMMAND The company command.
Optional argument ARG Matched prefix.
Optional argument IGNORED ignored."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-tailwindcss-backend))
    (prefix (and (member major-mode company-tailwindcss-major-modes)
                 (company-grab-symbol)))
    (candidates
     (cl-remove-if-not (lambda (c)
                         (string-prefix-p arg c t))
                       company-tailwindcss-classes))
    (sorted t)))

(add-to-list 'company-backends 'company-tailwindcss-backend)

(provide 'company-tailwindcss)

;;; company-tailwindcss.el ends here
