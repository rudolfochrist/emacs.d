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

(defun fyi/search-kb (search-string)
  (interactive "MSearch String: ")
  (grep (concat "grep --color -inH -C 3 -e "
                (shell-quote-argument search-string)
                " ~/org/kb.org"))
  (other-window 1))

(global-set-key (kbd "C-c C-c p") 'fyi/search-kb)


(provide 'init-defuns)
