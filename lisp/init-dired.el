(require 'dired-x)

(global-set-key (kbd "<f10>") 'dired-jump)
(global-set-key (kbd "<M-f10>")
                (lambda ()
                  (interactive)
                  (let ((current-prefix-arg '(4)))
                    (call-interactively #'dired-jump))))

;;; open eshell from dired
;;; http://oremacs.com/2015/01/10/dired-ansi-term/
(define-key dired-mode-map (kbd "`") 'multi-term)

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
(defun fyi-system-open-file ()
  (interactive)
  (shell-command-to-string (format "open %s" (shell-quote-argument (dired-get-file-for-visit)))))
(define-key dired-mode-map (kbd "M-RET") 'fyi-system-open-file)

(when (eq system-type 'darwin)
  (defun dired-open-in-finder ()
    (interactive)
    (shell-command (format "open %s" (dired-current-directory))))
  (define-key dired-mode-map (kbd "M-.") #'dired-open-in-finder))

;;; mc style move/rename in dired
(setq dired-dwim-target t)

;;; Copy file path to kill-ring
(defun fyi-file-path-to-kill ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (kill-new file)
    (message "Copied %s to kill-ring" file)))
(define-key dired-mode-map (kbd "C-c c") 'fyi-file-path-to-kill)

;;; use GNU Coreutils ls
(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls"))

;;; use natrual sort
(setq dired-listing-switches "-alv")

;;; don't ask recursively
;;; https://www.reddit.com/r/emacs/comments/3ncebl/in_the_case_if_anyone_wants_to_give_dired_a_try/cvn6eml
(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top)

;;; gnus attachment integration
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;;; dired-details
;;; inspired by https://github.com/magnars/.emacs.d/
(require-package 'dired-details)
(setq dired-details-hidden-string "--- ")
(dired-details-install)

;;; async copy many files
;;; http://oremacs.com/2016/02/24/dired-rsync/
(defun fyi-dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(with-eval-after-load "dired"
  (define-key dired-mode-map "Y" #'fyi-dired-rsync))

;;; dired-narrow
(require-package 'dired-narrow)

(with-eval-after-load "dired"
  (define-key dired-mode-map "/" #'dired-narrow))

(provide 'init-dired)
