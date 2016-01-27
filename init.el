;;; My emacs config. Inspired by https://github.com/bodil/emacs.d

;; use CL features
(require 'cl-lib)

;;; customization
(setq emacs-d-vendor (expand-file-name "~/.emacs.d/vendor/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; prefer newer files
(setq load-prefer-newer t)

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

(defun load-package (pkg require-symbol from-dir)
  "Install a package only if it's not already installed"
  (unless (package-installed-p pkg)
    (if from-dir
        (push from-dir load-path)
        (package-install pkg)))
  (if require-symbol
      require-symbol
      pkg))

(cl-defun require-package (pkg &key require from-dir filename noerror)
  (require (load-package pkg require from-dir) filename noerror))

;;; setup paths
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Subpackages
(setq setup-pkg-full
      '(init-basics
        init-defuns
        init-appearance
        init-linum
        init-my-override-keymap
        init-keybindings
        init-spelling
        init-company-mode
        init-hydra
        init-javascript
        init-markdown
        init-paredit
        init-slime
        init-magit
        init-hippie-expand
        init-multiple-cursors
        init-org-mode
        init-erlang
        init-yasnippet
        init-prolog
        init-eval-sexp-fu
        init-org-babel
        init-avy
        init-ace-window
        init-geiser
        init-shen
        init-eshell
        init-info
        init-ediff
        init-dired
        init-iedit
        init-emacs-lisp
        init-erc
        init-org-page
        init-term
        init-gnus
        init-whitespace
        init-easy-kill
        init-interleave
        init-undo-tree
        init-unkillable-scratch
        init-elisp-slime-nav
        init-ag
        init-swiper
        init-ivy
        init-macrostep
        init-highlight-backquotes-mode
        init-aggressive-indent-mode
        init-yaml-mode
        init-ruby
        init-haml-sass
        init-web-mode
        init-search-kb
        init-ledger-mode
        init-anzu
        init-beacon
        init-page-navigation
        init-redshank))
        init-projectile))

;; load them
(dolist (file setup-pkg-full)
  (require file))
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
