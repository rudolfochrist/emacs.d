(require 'dired-x)
(require 'eshell)

(global-set-key (kbd "<f10>") 'dired-jump)

;;; open eshell from dired
;;; http://oremacs.com/2015/01/10/dired-ansi-term/
(define-key dired-mode-map (kbd "`") 'fyi/ansi-term-zsh)

;;; ediff selected files
(define-key dired-mode-map (kbd "e") 'ediff-files)

;;; garbage files
(setq dired-garbage-files-regexp
      "\\.idx\\|\\.run\\.xml$\\|\\.bbl$\\|\\.bcf$\\|.blg$\\|-blx.bib$\\|.nav$\\|.snm$\\|.out$\\|.synctex.gz$\\|\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyg\\)\\)\\'")
(define-key dired-mode-map (kbd "%^") 'dired-flag-garbage-files)

;;; FIND
(define-key dired-mode-map (kbd "F") 'find-name-dired)

;;; resuse dired buffers
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")
  (lambda ()
    (interactive)
    (find-alternate-file "..")))

;;; open with default/system app
(defun fyi/system-open-file ()
  (interactive)
  (shell-command-to-string (format "open %s" (shell-quote-argument (dired-get-file-for-visit)))))
(define-key dired-mode-map (kbd "M-RET") 'fyi/system-open-file)

;;; mc style move/rename in dired
(setq dired-dwim-target t)

(provide 'init-dired)
