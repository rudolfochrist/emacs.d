(require-package 'ivy)
(require 'cl-lib)
(require-package 'counsel)
(require-package 'smex)
(require 'recentf)

(setq ivy-display-style 'fancy)
(ivy-mode 1)

(global-set-key (kbd "<f2>") #'counsel-ag)
(global-set-key (kbd "C-h f") #'counsel-describe-function)
(global-set-key (kbd "C-h v") #'counsel-describe-variable)
(global-set-key (kbd "<f6>") #'ivy-resume)
(global-set-key (kbd "M-x") #'counsel-M-x)
(define-key my-override-keymap-map (kbd "C-x C-S-f") #'ivy-recentf)

;;; [[http://endlessparentheses.com/visit-directory-inside-a-set-of-directories.html][Visit Directory inside a Set of Directories with Emacs]]
(defcustom fyi-favorite-directories
  '("~/prj/" "~/archive/" "~/.emacs.d/" "~/quicklisp/local-projects/"
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
(global-set-key (kbd "C-x C-d") #'fyi-visit-favorite-dir)

;;; personal counsel commands

(defvar counsel-makefile-pattern "\[M|m\]akefile"
  "Regex to match Makefile names.")

(defun counsel--extract-make-targets (&rest _ignore)
  (let* ((dir (file-name-directory (buffer-file-name)))
         (makefile (car (directory-files dir t counsel-makefile-pattern)))
         targets)
    (unless makefile
      (user-error "No Makefile in %s" dir))
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (while (re-search-forward "^\\w*\\s-*:\[ |\n\]" nil t)
        (push (string-trim (subseq (match-string 0) 0 (position ?: (match-string 0))))
              targets)))
    targets))

(defun counsel-make (target &rest flags)
  "Run GNU Make TRAGET.

FLAGS defaults to '-k'."
  (interactive
   (let ((flags "-k"))
     (when current-prefix-arg
       (setq flags (read-from-minibuffer "Flags: "
                                         nil
                                         read-expression-map
                                         nil
                                         read-expression-history)))
     (list (ivy-read "Target: " #'counsel--extract-make-targets)
           flags)))
  (compile (format "make %s %s"(mapconcat #'identity flags " ") target)))

(global-set-key (kbd "C-x tc") #'counsel-make)

(provide 'init-ivy)
