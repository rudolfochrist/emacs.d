(require-package 'page-break-lines)
(eval-when-compile
  (require 'hydra))

(global-page-break-lines-mode)

;;; [[http://endlessparentheses.com/improving-page-navigation.html][Improving page (section) navigation · Endless Parentheses]]
(defun fyi-page-navigation-recenter-advice (&rest ignored)
  "Recenter to start page."
  (when (called-interactively-p 'any)
    (recenter 5)))

(advice-add #'backward-page :after #'fyi-page-navigation-recenter-advice)
(advice-add #'forward-page :after #'fyi-page-navigation-recenter-advice)


;;;  see https://github.com/david-christiansen/helm-pages/blob/master/helm-pages.el
(defun counsel-pages--get-next-header ()
  (save-excursion
    (save-restriction
      (narrow-to-page)
      (beginning-of-line)
      (while (and (not (eobp))
                  (looking-at-p "^\\s-*$"))
        (forward-line))
      (let ((start (progn
                     (beginning-of-line)
                     (point)))
            (end (progn
                   (end-of-line)
                   (point))))
        (buffer-substring start end)))))

(defun counsel-pages--get-pages (&rest _ignored)
  "Collects the buffer's pages as (POS . HEADER) elements."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((pages (list (cons (counsel-pages--get-next-header) (point)))))
        (while (re-search-forward page-delimiter nil t)
          (push (cons (counsel-pages--get-next-header) (point))
                pages))
        (nreverse pages)))))

(defun counsel-pages ()
  (interactive)
  (ivy-read "Page: " (counsel-pages--get-pages)
            :action (lambda (point)
                      (goto-char point)
                      (recenter 5))))

(defhydra hyfra-page-navigation (global-map "C-x")
  "Page break navigation"
  ("'" counsel-pages "Pages" :color blue)
  ("[" backward-page "back")
  ("]" forward-page "forward")
  ("RET" nil "quit")
  ("q" nil "quit"))

(provide 'init-page-navigation)
