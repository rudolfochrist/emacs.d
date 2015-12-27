
;;; see https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings/358#358

(defvar my-override-keymap-map (make-sparse-keymap)
  "Keymap to hold all overridden key bindings.")

(define-minor-mode my-override-keymap
  "Minor mode to override key bindings in other [major|minor]-modes."
  nil
  nil
  my-override-keymap-map)

(my-override-keymap 1)

(defun my-override-keymap-load-advice (&rest _ignore)
  "Ensures that `my-override-keymap' map has precedence."
  (unless (eq (caar minor-mode-map-alist) 'my-override-keymap)
    (let ((my-keys (assq 'my-override-keymap minor-mode-map-alist)))
      (assq-delete-all 'my-override-keymap minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist my-keys))))

(advice-add 'load :after #'my-override-keymap-load-advice)

(define-globalized-minor-mode global-my-override-keymap
  my-override-keymap my-override-keymap)

(global-my-override-keymap 1)

;;; don't use it in the minibuffer.
(add-hook 'minibuffer-setup-hook
          (lambda () (my-override-keymap -1)))

(provide 'init-my-override-keymap)
