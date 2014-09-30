;;; My emacs config. Inspired by https://github.com/bodil/emacs.d

;; use CL features
(require 'cl)

;;; customization
(add-to-list 'load-path "~/.emacs.d/lisp")

;; homebrew executables
(add-to-list 'exec-path "/usr/local/bin/")

;; Latex
(add-to-list 'exec-path "/usr/texbin/")

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
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
        setup-w3m
        setup-sunrise-commander
        ))

;; load them
(dolist (file setup-pkg-full)
  (require file))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray13" :foreground "#bdbdb3" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "PT_Mono"))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold :height 1.0))))
 '(org-level-2 ((t (:foreground "#edd400" :weight bold :height 1.0)))))
(put 'dired-find-alternate-file 'disabled nil)
