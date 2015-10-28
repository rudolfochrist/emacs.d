(require-package 'bbdb)
(require-package 'bbdb-ext)
(require-package 'bbdb-vcard)
(require 'gnus)
(require 'nnir)

;;; see http://blog.binchen.org/posts/notes-on-using-gnus.html

;;; globals
(setq  gnus-home-score-file (expand-file-name "~/.emacs.d/all.SCORE")
       gnus-use-cache t
       ;; fetch only part of the article. If possible.
       gnus-read-active-file 'some
       ;; don't keep message buffers around
       message-kill-buffer-on-exit t
       gnus-use-correct-string-widths nil
       ;; don't show hmtl mail...
       mm-discouraged-alternatives '("text/html" "text/richtext")
       mm-automatic-display '("text/plain"
                              "text/enriched"
                              "text/x-verbatim"
                              "text/x-vcard"
                              "image/.*"
                              "message/delivery-status"
                              "multipart/.*"
                              "message/rfc822"
                              "text/x-patch"
                              "text/dns"
                              "application/pgp-signature"
                              "application/emacs-lisp"
                              "application/x-emacs-lisp"
                              "application/x-pkcs7-signature"
                              "application/pkcs7-signature"
                              "application/x-pkcs7-mime"
                              "application/pkcs7-mime"
                              "application/pgp\\'"
                              "text/x-org")
       gnus-treat-display-smileys nil)

;;; display inlined images
(add-to-list 'mm-attachment-override-types "image/.*")

;;; RSS [.newsrc synched therefore the primary select method]
(setq gnus-select-method
      '(nntp "news.gwene.org"
        (nntp-open-connection-function nntp-open-tls-stream)
        (nntp-port-number 563)
        (nntp-address "news.gwene.org")))

;;; news
(add-to-list 'gnus-secondary-select-methods
             '(nntp "nntp.aioe.org"
               (nntp-open-connection-function nntp-open-tls-stream)
               (nntp-port-number 563)
               (nntp-address "nntp.aioe.org")))

(add-to-list 'gnus-secondary-select-methods
             '(nntp "news.gmane.org"
               (nntp-open-connection-function nntp-open-tls-stream)
               (nntp-port-number 563)
               (nntp-address "news.gmane.org")))

;;; gmail
(add-to-list 'gnus-secondary-select-methods
             '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)
               (nnimap-search-engine imap)
               (nnimap-authinfo-file "~/.authinfo.gpg")))

;;; hooks
(defun fyi-gnus-multi-tab ()
  "bbdb mail complete in message header. Yasnippet expand in message body."
  (interactive)
  (if (message-in-body-p)
      (yas-expand)
      (bbdb-complete-mail)))

;;; citation
(setq sc-cite-blank-lines-p t
      sc-fixup-whitespace-p nil
      sc-auto-fill-region-p nil
      sc-citation-leader "  "
      sc-preferred-attribution-list '("x-attribution" "no-attrib") ; see below!
      sc-confirm-always-p nil
      sc-preferred-header-style 1)
(add-hook 'mail-citation-hook 'sc-cite-original)

(defun fyi-sc-pre-handler ()
  ;; don't use attribution if x-attribution is undefined
  ;; I confess: a little hacky
  (push '("no-attrib" . "") sc-attributions))
(add-hook 'sc-attribs-preselect-hook 'fyi-sc-pre-handler)

;;; message setup
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'message-mode-hook (lambda ()
                               (ispell-change-dictionary "german8")
                               (enable-yas-minor-mode)
                               (bbdb-mail-aliases)
                               (local-set-key (kbd "TAB") 'fyi-gnus-multi-tab)))

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
         (display . all)
         (expiry-wait . never))
        ("Sent Mail"
         (display . all))
        ("INBOX"
         (gnus-thread-sort-functions '(gnus-thread-sort-by-date))
         (total-expire . t)
         (expiry-wait . immediate))
        ("on-hold"
         (expiry-wait . immediate)
         (gcc-self . t))
        ("follow-up"
         (expiry-wait . immediate)
         (gcc-self . t))))

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
(bbdb-initialize '(gnus mail message pgp anniv))

;;; global key bindings
(global-set-key (kbd "<f11>") 'gnus)
(global-set-key (kbd "M-<f11>") 'gnus-other-frame)

;;; wash GWENE
(defun fyi-gwene-wash-html ()
  (when (string-prefix-p "gwene" gnus-newsgroup-name)
    (gnus-article-wash-html)))
(add-hook 'gnus-article-prepare-hook 'fyi-gwene-wash-html)

;;; RSS Reader features
(defun fyi-get-article-url ()
  (gnus-summary-verbose-headers 1)
  (prog1
      (with-current-buffer gnus-article-buffer
        (let ((nnmail-extra-headers (cons 'Archived-at
                                          nnmail-extra-headers)))
          (let ((archived-at (cdr (assoc 'Archived-at
                                         (mail-header-extra (nnheader-parse-head t))))))
            (when archived-at
              (substring archived-at 1 -1)))))
    (gnus-summary-verbose-headers -1)))

(defun fyi-gwene-browse-original ()
  (interactive)
  (browse-url (fyi-get-article-url)))

(defun fyi-gwene-read-later ()
  (interactive)
  (let ((title (mail-header-subject
                (gnus-summary-article-header
                 (gnus-summary-article-number))))
        (url (fyi-get-article-url))
        (org-capture-link-is-already-stored t))
    (push (list url title) org-stored-links)
    (org-store-link-props :link url
                          :description title
                          :annotation (org-make-link-string url title))
    (org-capture nil "r")))

(defhydra hydra-rss-reader (:color blue)
  "RSS Reader Convenience"
  ("o" fyi-gwene-browse-original "browse original")
  ("r" fyi-gwene-read-later "read later"))

(define-key gnus-summary-mode-map (kbd "C-c C-.") 'hydra-rss-reader/body)
(define-key gnus-article-mode-map (kbd "C-c C-.") 'hydra-rss-reader/body)

(provide 'init-gnus)
