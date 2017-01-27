(require 'info-look)
;;; Add custom documentation
(add-to-list 'Info-directory-list "~/info")

(global-set-key (kbd "C-h a") #'apropos)
(global-set-key (kbd "C-h A") #'info-apropos)

(provide 'init-info)
