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
        init-magit
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
        init-geiser
        init-shen
        init-eshell
        init-mu4e
        init-info
        init-ediff
        init-dired
        init-iedit
        init-sotlisp
        init-erc
        init-ido
        init-org-page
        init-term
        init-gnus
        init-whitespace
        ))

;; load them
(dolist (file setup-pkg-full)
  (require file))
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
