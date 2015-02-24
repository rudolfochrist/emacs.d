;;; My emacs config. Inspired by https://github.com/bodil/emacs.d

;; use CL features
(require 'cl-lib)

;;; customization
(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; homebrew executables
(add-to-list 'exec-path "/usr/local/bin/")

;; Latex
(add-to-list 'exec-path "/usr/texbin/")

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun load-package (pkg)
  "Install a package only if it's not already installed"
  (when (not (package-installed-p pkg))
    (package-install pkg))
  pkg)

(defun require-package (pkg)
  (require (load-package pkg)))

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
        init-expand-region
        init-magit
        init-hippie-expand-slime
        init-multiple-cursors
        init-org-mode
        init-erlang
        init-smex
        init-yasnippet
        init-ag
        init-undo-tree
        init-prolog
        init-eval-sexp-fu
        init-org-babel
        init-ace-jump
        init-geiser
        init-shen
        init-eshell
        init-mu4e
        init-info
        init-ediff
        init-dired
        init-iedit
        init-sotlisp
        init-swoop
        init-erc
        init-ido
        ))

;; load them
(dolist (file setup-pkg-full)
  (require file))
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
