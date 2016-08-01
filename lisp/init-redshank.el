(require-package 'redshank)
(require 'redshank-loader)

(defun redshank-let<->let* ()
  (interactive)
  (save-excursion
    (redshank-point-at-enclosing-let-form)
    (forward-char)
    (cond
     ((looking-at "let ")
      (forward-word)
      (insert "*"))
     ((looking-at "let\\* ")
      (forward-word)
      (delete-char 1)))))

(redshank-setup '(lisp-mode-hook
                  emacs-lisp-mode-hook
                  slime-repl-mode-hook))

(defun fyi-redshank-bindings ()
  (define-key redshank-mode-map [C-mouse-1] #'redshank-ignore-event)
  (define-key redshank-mode-map [C-down-mouse-1] #'redshank-copy-thing-at-point)
  (define-key redshank-mode-map [C-S-mouse-1] #'redshank-ignore-event)
  (define-key redshank-mode-map [C-S-down-mouse-1] #'redshank-generate-thing-at-point)
  (local-set-key (kbd "C-c C-.") #'hydra-redshank/body))

(add-hook 'redshank-mode-hook #'fyi-redshank-bindings)

(defhydra hydra-redshank (:color blue)
  "Modifications"
  ("c" redshank-condify-form "Condify")
  ("e" redshank-eval-whenify-form "Eval-Whenify")
  ("f" redshank-complete-form "Complete form")
  ("l" redshank-letify-form-up "Letify up")
  ("L" redshank-enclose-form-with-lambda "Enclose with λ")
  ("k" redshank-let<->let* "LET <-> LET*")
  ("n" redshank-rewrite-negated-predicate "Negate predicate")
  ("p" redshank-maybe-splice-progn "Maybe splice progn")
  ("x" redshank-extract-to-defun "Extract to defun")
  ("q" nil "Quit"))

(provide 'init-redshank)
