;(load-package 'darktooth-theme nil)
(load-package 'cyberpunk-theme nil)
(load-package 'flatui-theme nil)

(defun fyi/light-theme ()
  "Load light color scheme."
  (interactive)
  (disable-theme 'cyberpunk)
  (load-theme 'flatui t)
  (message "Light theme loaded."))

(defun fyi/dark-theme ()
  "Load dark color theme."
  (interactive)
  (disable-theme 'flatui)
  (load-theme 'cyberpunk t)
  (when (featurep 'smart-mode-line)
    (sml/apply-theme 'dark))
  (message "Dark theme loaded."))

(let ((hour (string-to-int (format-time-string "%H"))))
  (if (< 6 hour 18)
      (fyi/light-theme)
      (fyi/dark-theme)))


;; when in GUI emacs set size of frame
(when window-system
  (set-frame-size (selected-frame) 150 45))

;;; pretty symbols
(global-prettify-symbols-mode 1)

(provide 'init-color)
