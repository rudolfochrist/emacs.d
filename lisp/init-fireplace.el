(require-package 'fireplace)

(defun sexy-time ()
  (interactive)
  (fireplace 1)
  (sleep-for 1)
  (async-shell-command "afplay ~/Desktop/gaye.mp3")
  (delete-other-windows))

(provide 'init-fireplace)
