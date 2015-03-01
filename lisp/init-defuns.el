(defun fyi/format-json ()
  (interactive)
  (shell-command-on-region
   (mark) (point) "python -m json.tool" (buffer-name) t))

(defun fyi/format-xml ()
  (interactive)
  (shell-command-on-region
   (mark) (point) "xmllint --format -" (buffer-name) t))

(defun fyi/configure-flyspell (dictionary)
  "Applies flyspell settings"
  (ispell-change-dictionary dictionary)
  (set-input-method "german-prefix")
  (flyspell-mode 1))


(provide 'init-defuns)
