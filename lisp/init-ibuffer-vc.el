(require-package 'ibuffer-vc)
(require 'ibuffer)

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
         (name 18 18 :left :elide)
         " "
         (size 9 -1 :right)
         " "
         (mode 16 16 :left :elide)
         " "
         (vc-status 16 16 :left)
         " "
         filename-and-process)))

(defun fyi/enable-ibuffer-vc ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'alphabetic)
    (ibuffer-do-sort-by-alphabetic)))

(add-hook 'ibuffer-hook #'fyi/enable-ibuffer-vc)

(provide 'init-ibuffer-vc)
