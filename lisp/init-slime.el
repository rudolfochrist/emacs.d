(load (expand-file-name "~/quicklisp/slime-helper.el"))

;;; setup local hyperspec
;;; see (ql:quickload "clhs") for more information
(load (expand-file-name "~/quicklisp/clhs-use-local.el") t)

(add-hook 'lisp-mode-hook #'slime-mode)
(add-hook 'inferior-lisp-mode-hook #'inferior-slime-mode)

(setq slime-complete-symbol*-fancy t)
(setq slime-complete-symbol-function #'slime-fuzzy-complete-symbol)
(setq slime-startup-animation t)

;;; use UTF-8
(setq slime-net-coding-system 'utf-8-unix)

;;; Multiple Lisps
(setq slime-lisp-implementations
      '((sbcl ("/usr/local/bin/sbcl"))
        (ccl ("/usr/local/bin/ccl"))
        (allegro ("/Applications/AllegroCLexpress.app/Contents/Resources/alisp"))
        (clisp ("/usr/local/bin/clisp"))
        (abcl ("/usr/local/bin/abcl") :env ("PATH=/usr/local/bin:/usr/bin:$PATH"))))

;;; getting contrib fancy
(setq slime-contribs '(slime-fancy slime-banner slime-asdf))

;;; but disable autodoc
(setq slime-use-autodoc-mode nil)

;;; use slime-mode on asd files
(add-to-list 'auto-mode-alist '("\\.asd\\'" . lisp-mode))

;;; https://github.com/daimrod/Emacs-config/blob/master/config/config-slime.el
(define-key slime-mode-map (kbd "C-c /") 'slime-selector)
(define-key slime-repl-mode-map (kbd "C-c /") 'slime-selector)

;;; https://github.com/daimrod/Emacs-config/blob/master/config/config-slime.el
(defun dmd/dump-slime ()
  "Dump current SLIME instance to PWD/slime.img"
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window "*inferior-lisp*")
    (goto-char (point-min))
    (insert (format "(trivial-dump-core::sbcl-dump-image-slime %S)" (expand-file-name "slime.img")))
    (inferior-slime-return)))

;;; https://github.com/daimrod/Emacs-config/blob/master/config/config-slime.el
(defun dmd/load-slime ()
  "Load a previously saved SLIME image (see `dmd/dump-slime') named PWD/slime.img."
  (interactive)
  (slime-start :program "/usr/local/bin/sbcl"
               :program-args '("--core" "slime.img")
               :directory default-directory))

;;; https://github.com/daimrod/Emacs-config/blob/master/config/config-slime.el
;; Add a directory to asdf:*central-registry*
(defslime-repl-shortcut slime-repl-add-to-central-registry
  ("add-to-central-registry" "+a" "add")
  (:handler (lambda (directory)
              (interactive
               (list (expand-file-name (file-name-as-directory
                                        (read-directory-name
                                         "Add directory: "
                                         (slime-eval '(swank:default-directory))
                                         nil nil "")))))
              (insert "(cl:pushnew (cl:truename #P\"" directory "\") asdf:*central-registry* :test #'equal)")
              (slime-repl-send-input t)))
  (:one-liner "Add a directory to asdf:*central-registry*"))

(defun fyi/slime-save-before-compile (&rest args)
  "Save the current buffer before compilation."
  (interactive)
  (save-buffer))

(add-to-list 'slime-before-compile-functions 'fyi/slime-save-before-compile)

;;; Lookup documenation in Info hyperspec
;;; need either https://github.com/RobBlackwell/dpans2texi
;;; or the GNU Common Lips Info files [https://www.gnu.org/software/gcl/]
(defun fyi/hyperspec-info-lookup (symbol)
  (interactive (list (read-string "Lookup Hyperspec: " (thing-at-point 'symbol))))
  (condition-case nil
      (progn
        (info "(ansicl/ansicl)")
        (Info-index symbol))
    ('user-error
     (message "%s not found!" symbol)
     (Info-exit))))

(define-key slime-mode-map (kbd "C-c C-d i") #'fyi/hyperspec-info-lookup)

(provide 'init-slime)
