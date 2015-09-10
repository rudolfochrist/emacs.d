;; Always use UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; disable backups and auto-save
(setq backup-inhibited t
      auto-save-default nil)

;; make y and n sufficient
(defalias 'yes-or-no-p 'y-or-n-p)

;; Whenever an external process changes a file underneath emacs, and there
;; was no unsaved changes int the corresponding buffer, just revert its
;; content to reflect what's on-disk
(global-auto-revert-mode 1)

;; don't ring the bell
(setq ring-bell-function 'ignore)

;;; delete trailing whitespace before save
(add-hook 'before-save-hook (lambda ()
                              (delete-trailing-whitespace)))

;;; make script executable if it contains shebang
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;;; better scrolling
(setq scroll-preserve-screen-position 'always)

;;; special characters EVERYWHERE!!!!!!
(defun fyi-input-method ()
  (set-input-method "latin-9-prefix"))
(add-hook 'text-mode-hook 'fyi-input-method)

(provide 'init-basics)
