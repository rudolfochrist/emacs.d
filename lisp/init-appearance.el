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

;;; org-mode font-locking
(custom-set-faces
 '(org-link ((t (:underline t))))
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

;;; mode-line setup
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

(defun fyi-toggle-mode-line ()
  (interactive)
  (let ((active-bg "#7CAFC2")
        (inactive-bg "#E8E8E8")
        (fg "#181818")
        (barely-visible 20))
    (cond
      ((= barely-visible (face-attribute 'mode-line :height nil 'default))
       (set-face-attribute 'mode-line nil
                           :foreground fg
                           :background active-bg)
       (set-face-attribute 'mode-line-inactive nil
                           :foreground fg
                           :background inactive-bg)
       (set-faces-attribute '(mode-line
                              mode-line-inactive)
                            :height (face-attribute 'default :height)))
      (t
       (set-face-attribute 'mode-line nil
                           :foreground active-bg
                           :background active-bg)
       (set-face-attribute 'mode-line-inactive nil
                           :foreground inactive-bg
                           :background inactive-bg)
       (set-faces-attribute '(mode-line
                              mode-line-inactive)
                            :height barely-visible)))))
(global-set-key (kbd "C-x tt") 'fyi-toggle-mode-line)

;;; toggle mode-line on startup
(fyi-toggle-mode-line)

;; tabs, spaces and indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2
      require-final-newline t
      show-paren-style 'mixed)
(show-paren-mode 1)

(provide 'init-appearance)
