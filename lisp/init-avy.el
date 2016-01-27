(require-package 'avy)

(defhydra hydra-avy (:color blue)
  "Jump to"
  ("w" avy-goto-word-or-subword-1 "word/subword")
  ("c" avy-goto-char "character")
  ("l" avy-goto-line "line")
  ("a" avy-goto-word-0 "all words"))

(define-key my-override-keymap-map
  (kbd "M-s")
  (lambda (prefix)
    (interactive "P")
    (if prefix
        (hydra-avy/body)
      (call-interactively #'avy-goto-word-1))))

(provide 'init-avy)
