;;; My emacs config. Inspired by https://github.com/bodil/emacs.d

;; use CL features
(require 'cl-lib)

;;; customization
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; executables
(add-to-list 'exec-path "/usr/local/bin/")

;; Latex (is this platform independent?)
(add-to-list 'exec-path "/usr/texbin/")

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun load-package (pkg require-symbol)
  "Install a package only if it's not already installed"
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (if require-symbol
      require-symbol
    pkg))

(cl-defun require-package (pkg &key require)
  (require (load-package pkg require)))

;; Subpackages
(setq setup-pkg-full
      '(init-basics
        init-defuns
        init-color
        init-linum
        init-keybindings
        init-spelling
        init-javascript
        init-markdown
        init-paredit
        init-slime
        init-redshank
        init-magit
        init-hippie-expand
        init-hippie-expand-slime
        init-multiple-cursors
        init-org-mode
        init-erlang
        init-smex
        init-yasnippet
        init-ag
        init-prolog
        init-eval-sexp-fu
        init-org-babel
        init-ace-jump
        init-ace-window
        init-geiser
        init-shen
        init-eshell
        init-mu4e
        init-info
        init-ediff
        init-dired
        init-iedit
        init-emacs-lisp
        init-sotlisp
        init-erc
        init-ido
        init-org-page
        init-term
        init-gnus
        init-whitespace
        init-smart-mode-line
        init-easy-kill
        init-bug-hunter
        init-engine-mode
        init-interleave
        init-ibuffer-vc
        init-undo-tree
        init-unkillable-scratch
        ))

;; load them
(dolist (file setup-pkg-full)
  (require file))
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
