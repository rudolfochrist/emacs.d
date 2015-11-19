(require-package 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(setq ledger-highlight-xact-under-point nil
      ledger-reconcile-default-commodity "€"
      ledger-reconcile-default-date-format "%Y-%m-%d")

(provide 'init-ledger-mode)
