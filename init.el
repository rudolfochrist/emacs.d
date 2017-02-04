;;; My emacs config.
;; use CL features
(require 'cl-lib)

;;; customization
(setq emacs-d (expand-file-name "~/.emacs.d/")
      emacs-d-site-lisp (expand-file-name "site-lisp/" emacs-d))
(add-to-list 'load-path emacs-d-site-lisp)

;;; prefer newer files
(setq load-prefer-newer t)

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; setup paths
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;; load other files
;;; - basics
;;; - packages
;;; - org
;;; - gnus

(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)

