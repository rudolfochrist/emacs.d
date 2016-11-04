(require 'eshell)
(require 'em-smart)

;;; setup smart eshell
(setq eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "htop")))

(defun switch-eshell ()
  "Switch to eshell buffer or hide it if current buffer"
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (switch-to-buffer (second (buffer-list)))
    (eshell)))
(global-set-key (kbd "<f1>") #'switch-eshell)

(defun eshell/clear ()
  "Clears the eshell buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(provide 'init-eshell)
