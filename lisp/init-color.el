;(require-package 'sublime-themes)
;(require-package 'moe-theme)
;(require-package 'minimal-theme)
(load "heroku-theme/heroku-theme")

;;; heroku theme terminal adjustments
(add-hook 'term-mode-hook #'(lambda ()
                              (set-face-attribute 'term-color-blue nil
                                                  :foreground "DeepSkyBlue1")
                              (set-face-attribute 'term-color-green nil
                                                  :background "green4")))

;(load-theme 'minimal-light t)

;;; customize mode-line
(set-face-background 'mode-line "black")
(set-face-foreground 'mode-line "white")
(set-face-foreground 'mode-line-highlight "red")


;; when in GUI emacs set size of frame
(when window-system
  (set-frame-size (selected-frame) 150 45))

;; make the cursor red
(set-cursor-color "#cd0000")            ; works only in GUI emacs

;;; pretty symbols
(global-prettify-symbols-mode 1)

(provide 'init-color)
