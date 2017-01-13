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
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

;;; theme du jour
(load (expand-file-name "themes/irreal" user-emacs-directory))

;; set font in GUI
(when window-system
  (set-frame-font "input-12" nil t))

;;; set frame size
(push '(width . 185) default-frame-alist)
(push '(height . 53) default-frame-alist)

;; tabs, spaces and indentation
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

(provide 'init-appearance)
