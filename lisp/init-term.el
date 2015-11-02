(require-package 'multi-term)

(setq multi-term-program "/bin/zsh")

(defun fyi-term-mode-p (buffer)
  (eq 'term-mode
      (with-current-buffer buffer
        major-mode)))

(defun fyi-term-buffer-list ()
  (remove-if-not #'fyi-term-mode-p (buffer-list)))

(defun fyi-next-term-buffer (term-buffers)
  (let ((current-index (position (current-buffer) term-buffers)))
    (cond
      ((null current-index)
       (car term-buffers))
      (t
       (bury-buffer (current-buffer))
       (nth (mod (1+ current-index)
                 (length term-buffers))
            term-buffers)))))

(defun fyi-cycle-terminals ()
  (interactive)
  (let ((term-buffers (fyi-term-buffer-list)))
    (cond
      ((null term-buffers)
       (multi-term))
      ((= (length term-buffers) 1)
       (switch-to-buffer (car term-buffers)))
      (t
       (switch-to-buffer (fyi-next-term-buffer term-buffers))))))

(global-set-key (kbd "<f1>") 'fyi-cycle-terminals)
(global-set-key (kbd "M-<f1>") 'multi-term)

(setq term-bind-key-alist
      '(("C-c C-j" . term-line-mode)
        ("C-c C-k" . term-char-mode)
        ("C-c C-p" . multi-term-prev)
        ("C-c C-n" . multi-term-next)
        ("C-c C-c" . term-interrupt-subjob)
        ("C-c C-e" . term-send-esc)
        ("C-p" . previous-line)
        ("C-n" . next-line)
        ("C-s" . isearch-forward)
        ("C-r" . term-send-reverse-search-history)
        ("C-m" . term-send-return)
        ("C-y" . term-paste)
        ("M-f" . term-send-forward-word)
        ("M-b" . term-send-backward-word)
        ("M-o" . term-send-backspace)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("M-M" . term-send-forward-kill-word)
        ("M-N" . term-send-backward-kill-word)
        ("<M-backspace>" . term-send-backward-kill-word)
        ("M-r" . term-send-reverse-search-history)
        ("M-," . term-send-raw)))
(multi-term-keystroke-setup)

(when (equal system-type 'darwin)
  (defun fyi-iterm-current-location ()
    (interactive)
    (term-send-raw-string "open -a iTerm $PWD")
    (term-send-return))
  (push '("M-." . fyi-iterm-current-location) term-bind-key-alist))

(provide 'init-term)
