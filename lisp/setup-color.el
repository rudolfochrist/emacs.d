(require-package 'tangotango-theme)
(require-package 'ample-theme)
(require-package 'sublime-themes)
(require-package 'moe-theme)

(moe-dark)

;; when in GUI emacs set size of frame
(when window-system
  (set-frame-size (selected-frame) 150 45))

;; make the cursor red
(set-cursor-color "#cd0000")            ; works only in GUI emacs

(provide 'setup-color)
