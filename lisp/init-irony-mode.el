
;;; company
(require-package 'company-irony)
(with-eval-after-load "company"
  (add-to-list 'company-backend #'company-irony))

;;; flycheck
(require-package 'flycheck-irony)
(with-eval-after-load "flycheck"
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
(add-hook 'irony-mode-hook #'flycheck-mode)

;;; irony-eldoc
(require-package 'irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)

;;; GDB setup
(require 'gdb-mi)
(setq gdb-many-windows t)

(defun gdb-restore-comint-bindings ()
  (define-key gdb-inferior-io-mode-map (kbd "M-c M-c") #'comint-interrupt-subjob))
(add-hook 'gdb-inferior-io-mode-hook #'gdb-restore-comint-bindings)

(provide 'init-irony-mode)
