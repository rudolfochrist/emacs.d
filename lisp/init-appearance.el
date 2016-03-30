;; disable splash screen
(setq inhibit-startup-message t)

;;; put fortune in scratch buffer
;;; https://www.reddit.com/r/emacs/comments/4agorq/got_bored_of_the_initial_scratch_message_so/
(setq initial-scratch-message
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\n$" ""    ; remove trailing linebreak
         (shell-command-to-string "fortune lambda")))))

;;;; Disable some GUI stuff
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; theme du jour

(require-package 'meacupla-theme :noerror t)
(load-theme 'meacupla t)

;; set font in GUI
(when window-system
  (set-frame-font "input-12" nil t))

;;; set frame size
(push '(width . 150) default-frame-alist)
(push '(height . 45) default-frame-alist)

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
(setq rm-whitelist (mapconcat 'identity *rm-mode-regexps* "\\|")
      sml/theme 'dark)
(sml/setup)

(provide 'init-appearance)
