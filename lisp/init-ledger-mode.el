(require-package 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger\\'" . ledger-mode))

(setq ledger-use-iso-dates t
      ledger-reconcile-default-commodity "EUR"
      ledger-reconcile-default-date-format "%Y-%m-%d")

(provide 'init-ledger-mode)
