;; disable splash screen
(setq inhibit-startup-message t)

;;; remove scratch buffer message
(setq initial-scratch-message nil)

;;;; Disable some GUI stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; theme du jour
(require-package 'iodine-theme :noerror t)
(load-theme 'iodine t)

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

;;; mode-line globals
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

;;; which-function-mode
(which-func-mode 1)

;;; smart-mode-line
(require-package 'rich-minority)
(require-package 'smart-mode-line)
(defvar *rm-mode-regexps*
  '(" \\[.*\\]"                         ; slime package + CL implementaion
    )
  "Regular expressions to minor modes visible in the mode line.")
(setq rm-whitelist (mapconcat 'identity *rm-mode-regexps* "\\|"))
(sml/setup)

;;; face adjustments
(custom-set-faces
 '(which-func ((t :foreground "white"))))

(provide 'init-appearance)
