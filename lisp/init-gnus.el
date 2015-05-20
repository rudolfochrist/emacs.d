(require 'nnir)

;;; see http://blog.binchen.org/posts/notes-on-using-gnus.html

;;; globals
(setq gnus-select-method '(nnml "")
      gnus-home-score-file (expand-file-name "~/.emacs.d/total-scoring.SCORE")
      gnus-use-cache t
      ;; fetch only part of the article. If possible.
      gnus-read-active-file 'some
      ;; don't keep message buffers around
      message-kill-buffer-on-exit t
      gnus-use-correct-string-widths nil)

;;; setup paths
(setq  message-directory (expand-file-name "~/gnus/mail/")
       gnus-directory (expand-file-name "~/gnus/news/")
       nnfolder-directory (expand-file-name "~/gnus/mail/archive"))

;;; gmail
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnimap-search-engine imap)
               (nnimap-authinfo-file "~/.authinfo.gpg")
               ;; press 'E' to expire email
               (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
               (nnmail-expiry-wait 90)))

;;; news
(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gmane.org"
               (nntp-open-connection-function nntp-open-tls-stream)
               (nntp-port-number 563)
               (nntp-address "news.gmane.org")))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.eternal-september.org"
               (nntp-open-connection-function nntp-open-tls-stream)
               (nntp-port-number 563)
               (nntp-address "news.eternal-september.org")))

;;; hooks
(add-hook 'mail-citation-hook 'sc-cite-original)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'message-mode-hook
          (apply-partially 'fyi-configure-flyspell "german8"))

;;; Tree view for groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;; threading
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-thread-ignore-subject t)

;;; sorting
(setq gnus-parameters
      '(("Mail"
         (gnus-thread-sort-functions '((not gnus-thread-sort-by-date))))
        ("All Mail"
         (display . all))
        ("INBOX"
         (gnus-thread-sort-functions '(gnus-thread-sort-by-date)))))

;;; me
(setq user-full-name "Sebastian Christ"
      user-mail-address "rudolfo.christ@gmail.com")

;;; sending mail
;;; this need gnutls to be installed
(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnytls t
      smptmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtp-auth-crednetials (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtp-debug-info t)


(global-set-key (kbd "<f11>") 'gnus)

(provide 'init-gnus)
