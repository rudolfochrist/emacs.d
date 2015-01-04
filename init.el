;;; My emacs config. Inspired by https://github.com/bodil/emacs.d

;; use CL features
(require 'cl)

;;; customization
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; homebrew executables
(add-to-list 'exec-path "/usr/local/bin/")

;; Latex
(add-to-list 'exec-path "/usr/texbin/")

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun load-package (pkg)
  "Install a package only if it's not already installed"
  (when (not (package-installed-p pkg))
    (package-install pkg))
  pkg)

(defun require-package (pkg)
  (require (load-package pkg
)))

;; Subpackages
(setq setup-pkg-full
      '(setup-basics
        setup-defuns
        setup-color
        setup-linum
        setup-keybindings
        setup-spelling
        setup-javascript
        setup-markdown
        setup-paredit
        setup-slime
        setup-expand-region
        setup-magit
        setup-switch-window
        setup-hippie-expand-slime
        setup-multiple-cursors
        setup-web-mode
        setup-org-mode
        setup-erlang
        setup-smex
        setup-projectile
        setup-yasnippet
        setup-ag
        setup-clojure
        setup-undo-tree
        setup-prolog
        setup-eval-sexp-fu
        setup-org-babel
        setup-ace-jump
        setup-geiser
        setup-deft
        setup-shen
        setup-sunrise-commander
        setup-eshell
        setup-mu4e
        setup-company-mode
        ))

;; load them
(dolist (file setup-pkg-full)
  (require file))
