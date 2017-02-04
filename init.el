;;; My emacs config.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

;; use CL features
(require 'cl-lib)

;;; site-lisp setup
(setq  site-lisp-directory (expand-file-name "site-lisp/" user-emacs-directory))
(add-to-list 'load-path site-lisp-directory)
(let ((default-directory site-lisp-directory))
  (normal-top-level-add-subdirs-to-load-path))

;;; prefer newer files
(setq load-prefer-newer t)

;;; emacs customizations
;;; http://irreal.org/blog/?p=3765
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;;; setup paths
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;; setup use-package
(require 'diminish)
(require 'use-package)
(require 'bind-key)



;;; BASICS

(setq inhibit-startup-message t
      system-uses-terminfo nil                ; use Emacs terminfo
      backup-inhibited t                      ; disable backups
      auto-save-default nil                   ; no auto-save files
      ring-bell-function 'ignore              ; don't ring the bell
      scroll-preserve-screen-position 'always
      confirm-kill-emacs 'yes-or-no-p)

(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(delete-selection-mode 1)

;;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; set up environment details.
;;; especially Perl is bitchy about it if you run commands from within
;;; Emacs
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")

;;; disable GUI stuff
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; default theme with custom background
;;; see http://irreal.org/blog/?p=3900
(set-background-color "white smoke")
(add-to-list 'default-frame-alist '(background-color . "white smoke"))

;;; tabs, spaces, indentation, parens
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      require-final-newline t
      show-paren-style 'mixed)
(show-paren-mode 1)

;;; mode-line
(setq display-time-24hr-format t
      display-time-default-load-average nil
      display-time-day-and-date t
      display-battery-mode t)
(display-time-mode 1)
(which-function-mode 1)

;;; make scripts executable if shebang present
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(when (eql 'darwin system-type)
  ;; use input method because on macOS there is no compose key.
  (set-input-method "latin-9-prefix"))



;;; UTILITIES

(defun window-system-p ()
  "Returns non-nil if current Emacs is running with a window-system."
  window-system)

(defun system-macos-p ()
  "Returns non-nil if `system-type' is 'darwin"
  (eql 'darwin system-type))

(defun mode-buffer-list (mode)
  "Returns a list of buffers with major mode MODE."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (eql major-mode mode)))
   (buffer-list)))



;;; KEYBINDINGS
;;;
;;; M-s is for search
;;; M-g is for goto
;;; C-. prefix map is for personal bindings

;;; C-.
(defvar ctl-period-map nil)
(define-prefix-command 'ctl-period-map)
(bind-key "C-." #'ctl-period-map)

;;; easier other-window
(bind-key* "<C-return>" #'other-window)



;;; load other files
;;; - basics
(load-file (expand-file-name "packages.el" user-emacs-directory))
(load-file (expand-file-name "overrides.el" user-emacs-directory))
;;; - org
;;; - gnus


;;; Misc.

;;; copied from https://github.com/abo-abo/hydra/blob/master/hydra-examples.el
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defhydra hydra-scaling (:hint nil)
  "
Some global mappings.
_q_ : I just changed my mind. Quit.

     Text Scaling                Window Resizing
-------------------------  ----------------------------
_g_: increase text scale              _k_
_f_: decrease text scale           _h_     _l_
_0_: default text size                _j_
                           _=_: Balance windows
"
  ("q" nil)
  ("g" text-scale-increase)
  ("f" text-scale-decrease)
  ("0" (text-scale-adjust 0) :color blue)
  ("h" (hydra-move-splitter-left 5))
  ("j" (hydra-move-splitter-down 5))
  ("k" (hydra-move-splitter-up 5))
  ("l" (hydra-move-splitter-right 5))
  ("=" balance-windows :color blue))

(bind-key "C-. =" #'hydra-scaling/body)


;;; enable disabled commands
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)

