`;;; My emacs config. Inspired by https://github.com/bodil/emacs.d

;; use CL features
(require 'cl)

;; add emacs.d to load path
(add-to-list 'load-path "~/.emacs.d/")

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(defun require-package (pkg)
  "Install a package only if it's not already installed"
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (require pkg))

;; Subpackages
(setq setup-pkg-full
      '(setup-better-defaults
        setup-basics
        setup-color
        setup-keybindings
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
        setup-smart-tab))

;; load them
(dolist (file setup-pkg-full)
  (require file))
