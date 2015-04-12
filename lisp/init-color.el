;(load-package 'darktooth-theme nil)
(load-package 'cyberpunk-theme nil)
(load-theme 'cyberpunk t)

;; when in GUI emacs set size of frame
(when window-system
  (set-frame-size (selected-frame) 150 45))

;;; pretty symbols
(global-prettify-symbols-mode 1)

(provide 'init-color)
