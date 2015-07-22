(require-package 'avy)

(defhydra hydra-avy (:color blue)
  "Jump to"
  ("w" avy-goto-word-or-subword-1 "word/subword")
  ("c" avy-goto-char "character"))

(global-set-key
 (kbd "C-M-S-q")
 (lambda (prefix)
   (interactive "P")
   (if prefix
       (hydra-avy/body)
       (avy-goto-word-or-subword-1))))

(provide 'init-avy)
