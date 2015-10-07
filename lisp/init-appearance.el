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

;;; color-scheme -> https://github.com/chriskempson/base16
(custom-set-faces
 ;; basic faces
 '(italic ((t (:slant italic))))
 '(underline ((t :underline t)))
 '(isearch ((t :background "#86C1B9")))
 '(highlight ((t :background "#A1B56C")))
 ;; font locking
 '(font-lock-comment-delimiter-face ((t :foreground "#585858")))
 '(font-lock-comment-face ((t :foreground "#585858")))
 '(font-lock-function-name-face ((t :bold t)))
 '(font-lock-builtin-face ((t :bold t)))
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-doc-face ((t :slant italic)))
 '(font-lock-string-face ((t :slant italic)))
 ;; org-mode
 '(org-link ((t (:underline t :bold t :foreground "#AB4642"))))
 '(org-date-selected ((t (:background "#AB4642" :foreground "#F8F8F8"))))
 '(org-warning ((t (:foreground "#AB4642" :bold t))))
 '(org-verbatim ((t (:foreground "#7CAFC2"))))
 '(org-code ((t (:foreground "#7CAFC2"))))
 ;; linum-realtive
 '(linum-relative-current-face ((t :foreground "#A16946")))
 ;; magit
 '(magit-branch-current ((t :foreground "#7CAFC2"))))

;;; info faces
;; (set-face-attribute 'info-xref nil :underline t :weight 'bold)

;; set font in GUI
(when window-system
  (set-frame-font "input-12" nil t))

;;; set frame size
(push '(width . 150) default-frame-alist)
(push '(height . 45) default-frame-alist)

;;; pretty symbols
(global-prettify-symbols-mode 1)

;; tabs, spaces and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      require-final-newline t
      show-paren-style 'mixed)
(show-paren-mode 1)

(provide 'init-appearance)
