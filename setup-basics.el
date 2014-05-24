;;;; Disable some GUI stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; disable backups and auto-save
(setq backup-inhibited t)
(setq auto-save-default nil)

;; disable splash screen
(setq inhibit-startup-message t)

;; make y and n sufficient
(defalias 'yes-or-no-p 'y-or-n-p)

;; tabs, spaces and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq require-final-newline t)
(show-paren-mode 1)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes int the corresponding buffer, just revert its
;; content to reflect what's on-disk
(global-auto-revert-mode 1)

;; don't ring the bell
(setq ring-bell-function 'ignore)

;;; Setup line numbers
(require-package 'linum-relative)
(global-linum-mode 1)
(setq linum-relative-current-symbol "")

(provide 'setup-basics)
