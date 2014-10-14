;;; See gist for more information: https://gist.github.com/areina/3879626
(require 'mu4e)

;;; directories
(setq mu4e-maildir (expand-file-name "~/Maildir"))
(setq mu4e-drafts-folder "/Gmail/[Google Mail].Drafts")
(setq mu4e-sent-folder "/Gmail/[Google Mail].Sent Mail")
(setq mu4e-trash-folder "/Gmail/[Google Mail].Trash")

;;; don't save messages to Sent Messages, Gmail/IMAP will take care of this
(setq mu4e-sent-messages-behavior 'delete)

;;; some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/Gmail/INBOX" . ?i)
        ("/Gmail/[Google Mail].Starred" . ?s)
        ("/Gmail/[Google Mail].All Mail" . ?a)))

;;; allow for updating mail using `U' in the main view
(setq mu4e-get-mail-command "offlineimap")

;;; me
(setq user-mail-address "rudolfo.christ@gmail.com"
      user-full-name "Sebastian Christ")

;;; skip duplicates
(setq mu4e-headers-skip-duplicates t)

;;; sending mail
;;; this need gnutls to be installed
(require 'smtpmail)

(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnytls t
      smptmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtp-auth-crednetials (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtp-debug-info t)

;;; don't keep message buffers araound
(setq message-kill-buffer-on-exit t)

(provide 'setup-mu4e)

