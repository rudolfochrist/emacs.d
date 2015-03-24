(load-theme 'leuven t)

;; when in GUI emacs set size of frame
(when window-system
  (set-frame-size (selected-frame) 150 45))

;; make the cursor red
(set-cursor-color "#cd0000")            ; works only in GUI emacs

;;; pretty symbols
(global-prettify-symbols-mode 1)

(provide 'init-color)
