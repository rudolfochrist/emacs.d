
(setq prolog-program-name "/usr/local/bin/swipl")

(defun fyi/prolog-keybindings ()
  "Key bindings used in prolog mode"
  (local-set-key (kbd "C-x C-e") 'prolog-consult-predicate)
  (local-set-key (kbd "C-c C-k") 'prolog-consult-buffer))

(add-hook 'prolog-mode-hook 'fyi/prolog-keybindings)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(provide 'init-prolog)
