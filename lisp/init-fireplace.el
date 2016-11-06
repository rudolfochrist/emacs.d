(require-package 'fireplace)

(defvar sexy-time-track "gaye.mp3"
  "Sexy music to be played. 
Provide the file name of the track.")

(defvar sexy-time-buffer nil
  "The sexy time music buffer.")

(defvar sexy-time-buffer-name "*sexy-time*"
  "The sexy time buffer name.")

(defun sexy-time ()
  (interactive)
  (fireplace 1)
  (sleep-for 1)
  (setq sexy-time-buffer (get-buffer-create sexy-time-buffer-name))
  (let ((path (expand-file-name (format "~/.sexy-time/%s" sexy-time-track))))
    (async-shell-command (format "afplay %s" path) sexy-time-buffer sexy-time-buffer))
  (delete-other-windows))

(defun sexy-time-quit ()
  (delete-process (get-process "Shell"))
  (kill-buffer sexy-time-buffer))

(advice-add 'fireplace-off :after #'sexy-time-quit)

(provide 'init-fireplace)
