;; disable splash screen
(setq inhibit-startup-message t)

;;; remove scratch buffer message
(setq initial-scratch-message nil)

;;;; Disable some GUI stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; theme
(defun set-faces-attribute (faces &rest args)
  "Set ARGS on multiple faces"
  (dolist (face faces)
    (apply 'set-face-attribute face nil args)))

(require-package 'basic-theme)
(load-theme 'basic t)

;;; minimal font-locking and minor adjustments
;;; color-scheme -> https://github.com/chriskempson/base16
(set-faces-attribute '(font-lock-comment-delimiter-face
                       font-lock-comment-face)
                     :foreground "#585858")

(set-faces-attribute '(font-lock-function-name-face
                       font-lock-builtin-face
                       font-lock-keyword-face)
                     :weight 'bold)

;;; italic strings and docs
(set-faces-attribute '(font-lock-doc-face
                       font-lock-string-face)
                     :slant 'italic)

;;; match highlight to color scheme
(set-face-background 'highlight "#AB4642")

;;; other custom faces
(custom-set-faces
 ;; org-mode
 '(org-link ((t (:underline t))))
 '(org-date-selected ((t (:background "#AB4642" :foreground "#F8F8F8"))))
 ;; magit
 '(magit-branch-remote ((t (:foreground "#AB4642"))))
 '(magit-branch-local ((t (:inherit magit-branch-remote))))
 '(magit-branch-current ((t (:inherit magit-branch-remote))))
 '(magit-head ((t (:inherit magit-branch-remote)))))

;;; info faces
(set-face-attribute 'info-xref nil :underline t :weight 'bold)

;; when in GUI emacs set size of frame
(when window-system
  (set-frame-size (selected-frame) 150 45)
  (set-frame-font "input-12" nil t))

;;; pretty symbols
(global-prettify-symbols-mode 1)

;; tabs, spaces and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      require-final-newline t
      show-paren-style 'mixed)
(show-paren-mode 1)

(provide 'init-appearance)
