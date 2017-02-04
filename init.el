;;; My emacs config.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; use CL features
(require 'cl-lib)

;;; site-lisp setup
(setq  site-lisp-directory (expand-file-name "site-lisp/" user-emacs-directory))
(add-to-list 'load-path site-lisp-directory)
(let ((default-directory site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

;;; prefer newer files
(setq load-prefer-newer t)

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; setup paths
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;; setup use-package
(require 'diminish)
(require 'use-package)

;;; load other files
;;; - basics
(load-file (expand-file-name "packages.el" user-emacs-directory))
;;; - org
;;; - gnus


;;; enable disabled commands
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)

