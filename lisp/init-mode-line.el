;;; general mode-line customizations

(defun set-faces-attribute (faces &rest args)
  "Set ARGS on multiple faces"
  (dolist (face faces)
    (apply 'set-face-attribute face nil args)))

(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time-mode 1)
(display-battery-mode 1)

;;; color, colors, colors
(defvar fyi-modeline-modified-bg "#AB4642"
  "The mode-line color when buffer is unsaved.")

(defvar fyi-modeline-default-bg "#7CAFC2"
  "The initial/default mode-line color.")

(defvar fyi-modeline-inactive-bg "#E8E8E8"
  "The inactive mode-line color.")

(defvar fyi-modeline-fg "#181818"
  "The mode-line foreground color. If `fyi-modeline-collapsed-p'.")

(defvar fyi-modeline-collapsed-height 20
  "The height of the mode-line in it's collapsed state.")

(defvar-local fyi-modeline-active-bg fyi-modeline-default-bg
  "The current background color.")

(defun fyi-modeline-collapsed-p ()
  "True if mode-line has a height of `fyi-modeline-collapsed-height'."
  (= fyi-modeline-collapsed-height
     (face-attribute 'mode-line :height nil 'default)))

(defun fyi-toggle-modeline ()
  "Expands/collapses the mode-line."
  (interactive)
  (setq face-remapping-alist (assq-delete-all 'mode-line face-remapping-alist))
  (cond
    ((fyi-modeline-collapsed-p)
     (set-faces-attribute '(mode-line mode-line-inactive)
                          :height (face-attribute 'default :height)
                          :foreground fyi-modeline-fg)
     (face-remap-add-relative 'mode-line `((:background ,fyi-modeline-active-bg)
                                           (:foreground ,fyi-modeline-fg)))
     (set-face-background 'mode-line-inactive fyi-modeline-inactive-bg))
    (t
     (set-faces-attribute '(mode-line mode-line-inactive)
                          :height fyi-modeline-collapsed-height)
     (face-remap-add-relative 'mode-line
                              `((:background ,fyi-modeline-active-bg)
                                (:foreground ,fyi-modeline-active-bg)))
     (set-face-attribute 'mode-line-inactive nil
                         :foreground fyi-modeline-inactive-bg
                         :background fyi-modeline-inactive-bg))))

(global-set-key (kbd "C-x tt") 'fyi-toggle-modeline)

(defun fyi-modeline-redraw (&rest _ignored)
  (setq face-remapping-alist (assq-delete-all 'mode-line face-remapping-alist))
  (face-remap-add-relative 'mode-line `((:background ,fyi-modeline-active-bg)))
  (when (fyi-modeline-collapsed-p)
    (face-remap-add-relative 'mode-line `((:foreground ,fyi-modeline-active-bg)))))

(defun fyi-modeline-mark-modified ()
  (setq fyi-modeline-active-bg fyi-modeline-modified-bg)
  (fyi-modeline-redraw))

(defun fyi-modeline-mark-saved (&rest _ignored)
  (setq fyi-modeline-active-bg fyi-modeline-default-bg)
  (fyi-modeline-redraw))

;;; registering
(add-hook 'after-save-hook 'fyi-modeline-mark-saved)
(add-hook 'after-revert-hook 'fyi-modeline-mark-saved)
(add-hook 'gnus-after-exiting-gnus-hook 'fyi-modeline-redraw)
(add-hook 'first-change-hook 'fyi-modeline-mark-modified)
(advice-add 'select-window :after 'fyi-modeline-redraw)
(advice-add 'undo :after (lambda (&rest _ignored)
                           (unless (buffer-modified-p)
                             (setq fyi-modeline-active-bg fyi-modeline-default-bg)
                             (fyi-modeline-redraw))))

;;; toggle mode-line on startup
(add-hook 'after-init-hook 'fyi-toggle-modeline)

(provide 'init-mode-line)
