;;; dot-org.el --- My Org Mode Configuration File

;;; Commentary:
;;
;; Title says it all :]

;;; Code:

;;; this has to be set before org.el is loaded
;;; EDIT 2022-03-22: Really?
(defvar org-export-backends '(ascii html latex))

(use-package org
  :ensure t)

(require 'org-export
         (expand-file-name
          "site-lisp/org-export.el"
          user-emacs-directory))
(require 'org-tempo) ; brings back the easy-templates

;;; org basics
(setq org-startup-indented t
      org-image-actual-width '(450)
      org-startup-with-inline-images t
      org-directory "~/org/"
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

;;; minor modes in org buffers
(add-to-list 'org-mode-hook
             (lambda ()
               (visual-line-mode 1)))

;;; limit the width
(defun fyi-org-width ()
  "Limit tthw buffer width fixed to 110."
  (set-fill-column 110)
  (auto-fill-mode t))
(add-hook 'org-mode-hook 'fyi-org-width)

;;; use org-special-* keys
(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-special-ctrl-o t)

;;; org/ispell ignorables
;;; see: http://endlessparentheses.com/ispell-and-org-mode.html
(defun fyi-org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'fyi-org-ispell)

;;; Links and navigation

(bind-key "C-c l" #'org-store-link org-mode-map)
(bind-key "M-," #'org-mark-ring-goto org-mode-map) ; navigate back after following a link

;;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp .t)
   (js . t)
   (lisp . t)
   (latex . t)
   (shell . t)
   (dot . t)))


(provide 'dot-org)

;;; dot-org.el ends here
