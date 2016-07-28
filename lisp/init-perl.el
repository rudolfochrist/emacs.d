
(defvar perl-repl-buffer-name "perl-repl"
  "Perl REPL buffer name")

(defvar perl-repl-command "tinyrepl")

(defun run-perl (arg)
  (interactive "P")
  (let ((buffer (or (get-buffer perl-repl-buffer-name)
                    (make-comint perl-repl-buffer-name perl-repl-command))))
    (if arg
        (switch-to-buffer buffer)
      (switch-to-buffer-other-window buffer))))

(defun perl-repl-eval (str)
  (comint-send-string (get-buffer-process (format "*%s*" perl-repl-buffer-name))
                      (concat str "\n")))

(defun perl-repl-eval-region (start end)
  (interactive "r")
  (perl-repl-eval (buffer-substring start end)))

(defun perl-repl-eval-buffer ()
  (interactive)
  (perl-repl-eval (buffer-string)))

(defun perl-repl-keybindings ()
  (local-set-key (kbd "C-c C-c") #'perl-repl-eval-buffer)
  (local-set-key (kbd "C-c C-r") #'perl-repl-eval-region))

(add-hook 'perl-mode-hook #'perl-repl-keybindings)

(provide 'init-perl)
