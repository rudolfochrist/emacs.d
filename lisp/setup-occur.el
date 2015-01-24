(eval-when-compile
  (require 'cl))

(defun fyi/buffers-matching-mode (mode)
  "Eavluates to a list of buffers with major-mode of MODE"
  (remove-if-not #'(lambda (buf)
                     (with-current-buffer buf
                       (if (equal mode major-mode)
                           buf
                         nil)))
                 (buffer-list)))

(defun fyi/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (fyi/buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(global-set-key (kbd "<M-f2>") 'fyi/multi-occur-in-this-mode)

(provide 'setup-occur)
