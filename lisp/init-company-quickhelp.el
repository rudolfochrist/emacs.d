(require-package 'pos-tip)
(require-package 'company-quickhelp)

(company-quickhelp-mode 1)

(setq company-quickhelp-delay nil)

(with-eval-after-load "company"
  (define-key company-active-map (kbd "C-h") #'company-quickhelp-manual-begin))

(provide 'init-company-quickhelp)
