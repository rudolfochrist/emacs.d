(require-package 'irony)

;;; lang modes
(add-hook 'c++-mode-hook #'irony-mode)
(add-hook 'objc-mode-hook #'irony-mode)
(add-hook 'c-mode-hook #'irony-mode)

(defun fyi-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    #'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    #'irony-completion-at-point-async))

(add-hook 'irony-mode-hook #'fyi-irony-mode-hook)
(add-hook 'irony-mode-hook #'irony-cdb-autosetup-compile-options)

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
