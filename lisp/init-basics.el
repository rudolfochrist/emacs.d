;;;; Disable some GUI stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Always use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; disable backups and auto-save
(setq backup-inhibited t)
(setq auto-save-default nil)

;; disable splash screen
(setq inhibit-startup-message t)

;;; remove scratch buffer message
(setq initial-scratch-message nil)

;; make y and n sufficient
(defalias 'yes-or-no-p 'y-or-n-p)

;; tabs, spaces and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq require-final-newline t)
(show-paren-mode 1)
(setq show-paren-style 'expression)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes int the corresponding buffer, just revert its
;; content to reflect what's on-disk
(global-auto-revert-mode 1)

;; don't ring the bell
(setq ring-bell-function 'ignore)

;;; user vertical ido and better flex matching
(require-package 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)

;;; delete trailing whitespace before save
(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)))

;;; put time and battery in status line
(setq display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

;;; make script executable if it contains shebang
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(provide 'init-basics)
