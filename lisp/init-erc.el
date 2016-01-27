(require 'erc)

(setq erc-nick "rudolfochrist"
      erc-prompt-for-password nil
      erc-user-full-name "Sebastian (Rudolfo) Christ"
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#lisp" "#lispcafe"))
      erc-hide-list '("JOIN" "PART" "QUIT"))

;;; enable logging
(require 'erc-log)
(setq erc-log-channels-directory "~/.erc/logs/")
(erc-log-enable)

;;; cyvle erc buffers
(defun fyi-erc-mode-p (buffer)
  "Tests if buffer is in erc-mode"
  (with-current-buffer buffer
    (eq major-mode 'erc-mode)))

(defun fyi-erc-buffers ()
  "List all erc buffers"
  (remove-if-not #'fyi-erc-mode-p (buffer-list)))

(defun fyi-cycle-erc-buffers ()
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (first (fyi-erc-buffers))))

;;; mark as read
;;; see: http://endlessparentheses.com/marking-emacs-chat-buffers-as-read.html?source=rss#disqus_thread
(defun fyi-mark-read ()
  "Mark buffer as read up to current line."
  (interactive)
  (let ((inhibit-read-only t))
    (put-text-property
     (point-min) (line-beginning-position)
     'face       'font-lock-comment-face)))

(with-eval-after-load 'erc
  (define-key erc-mode-map (kbd "C-`") #'fyi-cycle-erc-buffers)
  (define-key erc-mode-map (kbd "<escape>") #'fyi-mark-read))

(provide 'init-erc)
