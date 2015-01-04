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
        ("/Gmail/[Google Mail].All Mail" . ?a)
        ("/Gmail/[Google Mail].Sent Mail" . ?s)
        ("/Gmail/on-hold" . ?h)
        ("/Gmail/follow-up" . ?f)))

;;; allow for updating mail using `U' in the main view
(setq mu4e-get-mail-command "offlineimap")

;;; some useful settings
(setq mu4e-attachment-dir "~/Downloads/")
(setq mu4e-confirm-quit nil)

;; show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses t)

;;; refiling
(setq mu4e-refile-folder "/Gmail/[Google Mail].All Mail")

;;; me
(setq user-mail-address "rudolfo.christ@gmail.com"
      user-full-name "Sebastian Christ")
(setq mu4e-user-mail-address-list '("rudolfo.christ@gmail.com"
                                    "sebastian@macrudy.com"
                                    "sebastian.christ@stud.hs-mannheim.de"))

;;; skip duplicates
(setq mu4e-headers-skip-duplicates t)

;;; view HTML emails in the browser
(add-to-list 'mu4e-view-actions '("browser email" . mu4e-action-view-in-browser) t)

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

;;; flyspell and german input in compose
(defun fyi/compose-settings ()
  (ispell-change-dictionary "german")
  (set-input-method "german-prefix")
  (flyspell-mode 1))

(add-hook 'mu4e-compose-mode-hook 'fyi/compose-settings)

(provide 'setup-mu4e)
