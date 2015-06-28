(require-package 'bbdb)
(require-package 'bbdb-ext)
(require-package 'bbdb-vcard)
(require 'gnus)
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
      gnus-use-correct-string-widths nil
      mm-text-html-renderer 'gnus-w3m)

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
               (nnmail-expiry-wait 'immediate)))

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
(add-hook 'message-mode-hook (lambda ()
                               (fyi-configure-flyspell "german8")
                               (bbdb-mail-aliases)
                               (local-set-key (kbd "TAB") 'bbdb-complete-mail)))

;;; Tree view for groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;; threading
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject
      gnus-summary-make-false-root 'dummy
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-thread-ignore-subject t)

;;; sorting
(setq gnus-parameters
      '(("Mail"                         ; topic
         (gnus-thread-sort-functions '((not gnus-thread-sort-by-date))))
        ("All Mail"                     ; group
         (display . all))
        ("INBOX"                        ; group
         (gnus-thread-sort-functions '(gnus-thread-sort-by-date)))))

;;; aesthetics
;;; https://github.com/vanicat/emacs24-starter-kit/blob/master/starter-kit-gnus.org#more-attractive-summary-view
(when window-system
  (setq gnus-sum-thread-tree-indent "  "
        gnus-sum-thread-tree-root "● "
        gnus-sum-thread-tree-false-root "◯ "
        gnus-sum-thread-tree-single-indent  ""
        gnus-sum-thread-tree-vertical        "│"
        gnus-sum-thread-tree-leaf-with-other "├─► "
        gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "│" "%&user-date;" "%23=│" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)

;;; gravatar
(setq gnus-treat-from-gravatar 'head
      gnus-treat-mail-gravatar 'head)

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


;;; managing contantcs
(setq bbdb-offer-save 'auto
      bbdb-notice-auto-save-file t
      bbdb-expand-mail-aliases t
      bbdb-canonicalize-redundant-nets-p t
      bbdb-always-add-addresses t
      bbdb-complete-mail-allow-cycling t)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-initialize '(gnus mail message pgp anniv))

(global-set-key (kbd "<f11>") 'gnus)

(provide 'init-gnus)
