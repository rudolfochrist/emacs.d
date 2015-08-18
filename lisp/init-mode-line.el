;;; general mode-line customizations
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

;;; color, colors, colors
(defvar fyi-modeline-edited-bg "#BA8BAF"
  "The mode-line color when buffer is unsaved.")
(defvar fyi-modeline-saved-bg "#7CAFC2"
  "The initial/default mode-line color.")
(defvar fyi-modeline-inactive-bg "#E8E8E8"
  "The inactive mode-line color.")
(defvar fyi-modeline-fg "#181818"
  "The mode-line foreground color. If `fyi-modeline-collapsed-p'.")
(defvar fyi-modeline-collapsed-height 20
  "The height of the mode-line in it's collapsed state.")

(defun fyi-modeline-active-bg ()
  "The active mode-line color based on `buffer-modified-p'."
  (if (buffer-modified-p)
      fyi-modeline-edited-bg
      fyi-modeline-saved-bg))

(defun fyi-modeline-collapsed-p ()
  "True if mode-line has a height of `fyi-modeline-collapsed-height'."
  (= fyi-modeline-collapsed-height
     (face-attribute 'mode-line :height nil 'default)))

(defun fyi-toggle-modeline ()
       "Expands/collapses the mode-line."
       (interactive)
       (cond
         ((fyi-modeline-collapsed-p)
          (set-faces-attribute '(mode-line mode-line-inactive)
                               :height (face-attribute 'default :height)
                               :foreground fyi-modeline-fg)
          (set-face-background 'mode-line (fyi-modeline-active-bg))
          (set-face-background 'mode-line-inactive fyi-modeline-inactive-bg))
         (t
          (set-faces-attribute '(mode-line mode-line-inactive)
                               :height fyi-modeline-collapsed-height)
          (set-face-attribute 'mode-line nil
                              :foreground (fyi-modeline-active-bg)
                              :background (fyi-modeline-active-bg))
          (set-face-attribute 'mode-line-inactive nil
                              :foreground fyi-modeline-inactive-bg
                              :background fyi-modeline-inactive-bg))))
(global-set-key (kbd "C-x tt") 'fyi-toggle-modeline)

(defun fyi-modeline-change-color (&rest _ignored)
  (set-face-background 'mode-line (fyi-modeline-active-bg)))

;;; OMG! Three different possibilities to accomplish the same behavior.
(add-hook 'after-save-hook 'fyi-modeline-change-color)
(push 'fyi-modeline-change-color after-change-functions)
(advice-add 'select-window :after 'fyi-modeline-change-color)

;;; toggle mode-line on startup
(fyi-toggle-modeline)

(provide 'init-mode-line)
