
;;; Setup line numbers
(global-linum-mode 1)

;;; disable linum in some major modes
(require-package 'linum-off
                 :from-dir emacs-d-site-lisp)
(setq linum-disabled-modes-list
      '(eshell-mode
        wl-summary-mode
        compilation-mode
        dired-mode
        doc-view-mode
        image-mode))

(defun disable-linum-on-huge-file ()
  "Disable linum if buffer contains more than 5000 lines.

This requires wc to be installed. Uses wc -c file for performace reason.

Ref: http://blog.binchen.org/posts/turn-off-linum-mode-when-file-is-too-big.html"
  (when (and (executable-find "wc")
             (> (string-to-number (shell-command-to-string (format "wc -c %s" (buffer-file-name))))
                (* 5000 80))
             (linum-mode -1))))
(add-hook 'prog-mode-hook #'disable-linum-on-huge-file)
(add-hook 'text-mode-hook #'disable-linum-on-huge-file)

(provide 'init-linum)
