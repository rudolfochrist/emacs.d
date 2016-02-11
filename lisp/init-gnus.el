(require-package 'bbdb)
(require-package 'bbdb-ext)
(require-package 'bbdb-vcard)
(require 'gnus)
(require 'nnir)

;;; see http://blog.binchen.org/posts/notes-on-using-gnus.html

;;; Settings highly influenced by John Wiegley (https://github.com/jwiegley/dot-emacs/)

;;; globals
(setq gnus-use-cache t
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
      gnus-treat-display-smileys nil
      gnus-gcc-mark-as-read t
      gnus-message-archive-group nil
      gnus-topic-display-empty-topics t)

(defun activate-gnus ()
  "Start Gnus when not already running."
  (unless (get-buffer "*Group*")
    (gnus)))

;;; RSS [.newsrc synced therefore the primary select method]
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

;;; finding parents
(setq gnus-refer-thread-use-nnir t
      gnus-refer-article-method
      '(current
        (nnir "nnimap:gmail")
        (nntp "news.gmane.org"
              (nntp-address "news.gmane.org"))
        (nntp "nntp.aioe.org"
              (nntp-address "nntp.aioe.org"))))

;;; message-mode setup
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

(add-hook 'message-mode-hook (lambda ()
                               (ispell-change-dictionary "german8")
                               (enable-yas-minor-mode)
                               (bbdb-mail-aliases)
                               (local-set-key (kbd "TAB") 'fyi-gnus-multi-tab)))

;;; Tree view for groups
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;; threading
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
      gnus-summary-gather-subject-limit 'fuzzy
      gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date
      gnus-thread-ignore-subject nil
      gnus-thread-hide-subtree t
      gnus-thread-sort-functions '((not gnus-thread-sort-by-most-recent-date)
                                   gnus-thread-sort-by-total-score)
      gnus-article-sort-functions '((not gnus-article-sort-by-most-recent-date)
                                    gnus-article-sort-by-score))

(add-hook 'gnus-summary-prepared-hook #'gnus-summary-hide-all-threads)

;;; parameters
(setq gnus-parameters
      '(("All Mail"
         (display . all)
         (expiry-wait . never)
         (gnus-article-sort-functions '(gnus-article-sort-by-most-recent-date))
         (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)))
        ("Sent Mail"
         (display . all))
        ("INBOX"
         (total-expire . t)
         (expiry-wait . immediate))
        ("Mail\\|INBOX"
         (gnus-use-scoring nil))
        ("on-hold"
         (expiry-wait . immediate)
         (gcc-self . t)
         (display . all))
        ("follow-up"
         (expiry-wait . immediate)
         (gcc-self . t)
         (display . all))
        ("gwene\\..*"
         (gnus-article-sort-functions '((not gnus-article-sort-by-most-recent-number)))
         (gnus-thread-sort-functions '((not gnus-thread-sort-by-most-recent-date)))
         (gnus-use-scoring nil))))

;;; Aesthetics
;;; https://github.com/vanicat/emacs24-starter-kit/blob/master/starter-kit-gnus.org#more-attractive-summary-view
(when window-system
  (setq gnus-sum-thread-tree-indent " "
        gnus-sum-thread-tree-root ""
        gnus-sum-thread-tree-false-root ""
        gnus-sum-thread-tree-single-indent ""
        gnus-sum-thread-tree-vertical "|"
        gnus-sum-thread-tree-leaf-with-other "+-> "
        gnus-sum-thread-tree-single-leaf "\\-> "))

(setq gnus-summary-line-format "%6V %U%R%O %-20&user-date; %-25,25f %3t %(%* %B%s%)\n"
      gnus-summary-display-arrow t)

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
      bbdb-complete-mail-allow-cycling t
      ;; don't handle anniversaries in BBDB.
      bbdb-anniv-alist nil)
(bbdb-initialize 'gnus 'message 'anniv)

;;; global key bindings
(global-set-key (kbd "<f11>") 'gnus)
(global-set-key (kbd "M-<f11>") 'gnus-other-frame)

;;; wash GWENE
(defun fyi-gwene-wash-html ()
  (when (string-prefix-p "gwene" gnus-newsgroup-name)
    (gnus-article-wash-html)))
(add-hook 'gnus-article-prepare-hook 'fyi-gwene-wash-html)

;;; URL handling features
;;; see https://github.com/jwiegley/dot-emacs/blob/master/dot-gnus.el
(defun gnus-article-get-urls-region (min max)
  "Return a list of urls found in the region between MIN and MAX."
  (let (url-list)
    (save-excursion
      (save-restriction
        (narrow-to-region min max)
        (goto-char (point-min))
        (while (re-search-forward gnus-button-url-regexp nil t)
          (let ((match-string (match-string-no-properties 0)))
            (if (and (not (equal (substring match-string 0 4) "file"))
                     (not (member match-string url-list)))
                (setq url-list (cons match-string url-list)))))))
    url-list))

(defun gnus-article-get-current-urls ()
  "Return a list of the urls found in the current `gnus-article-buffer'."
  (let (url-list)
    (with-current-buffer gnus-article-buffer
      (setq url-list
            (gnus-article-get-urls-region (point-min) (point-max))))
    url-list))

(defun gnus-article-urls-action (action &optional support)
  "Applies ACTION on the found URLs in article buffer."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-select-article nil nil 'pseudo)
  (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
        (urls (gnus-article-get-current-urls))
        (this-window (selected-window))
        (browse-window (get-buffer-window gnus-article-buffer))
        (count 0))
    (save-excursion
      (when (> (length urls) 1)
        (save-window-excursion
          (with-current-buffer temp-buffer
            (mapc (lambda (string)
                    (insert (format "\t%d: %s\n" count string))
                    (setq count (1+ count))) urls)
            (not-modified)
            (pop-to-buffer temp-buffer)
            (setq count
                  (string-to-number
                   (char-to-string (if (fboundp
                                        'read-char-exclusive)
                                       (read-char-exclusive)
                                     (read-char)))))
            (kill-buffer temp-buffer))))
      (if browse-window
          (progn (select-window browse-window)
                 (funcall action
                          (nth count urls)
                          (when support
                            (funcall support))))))
    (select-window this-window)))

(defun fyi-article-get-header (header &optional skip-bounds)
  "Return HEADER of article in the current `gnus-article-buffer'.

If SKIP-BOUNDS is non-nil, skip the first and the last character from the header
value. This is useful if you want to omit some '<' and '>' that some headers
have (e.g. Message-ID)."
  (gnus-summary-verbose-headers 1)
  (prog1
      (with-current-buffer gnus-article-buffer
        (let ((nnmail-extra-headers (cons header
                                          nnmail-extra-headers)))
          (let ((header-value (cdr (assoc header
                                          (mail-header-extra (nnheader-parse-head t))))))
            (when header-value
              (apply #'substring-no-properties
                     header-value
                     (when skip-bounds
                       (list 1 -1)))))))
    (gnus-summary-verbose-headers -1)))

(defun fyi-article-archived-at ()
  "Return archived-at header of article in the current `gnus-article-buffer'."
  (fyi-article-get-header 'Archived-at t))

(defun fyi-article-message-id ()
  "Return the article's Message-ID."
  (fyi-article-get-header 'Message-ID))

(defun fyi-article-subject ()
  (fyi-article-get-header 'Subject))

(defun fyi-article-browse-original ()
  "Open current article in the browser."
  (interactive)
  (browse-url (or (fyi-article-archived-at)
                  (fyi-article-message-id))))

(defun fyi-capture-read-later (url &optional title)
  "Capture URL to read-later file.

If TITLE is nil, then the URL is used as title."
  (let ((org-capture-link-is-already-stored t))
    (push (list url (or title url)) org-stored-links)
    (org-store-link-props :link url
                          :description (or title url)
                          :annotation (org-make-link-string url (or title url)))
    (org-capture nil "r")))

(defun fyi-article-read-later ()
  "Save article to read-later."
  (interactive)
  (fyi-capture-read-later (or (fyi-article-archived-at)
                              (fyi-article-message-id))
                          (fyi-article-subject)))

(defhydra hydra-gnus (:color blue :hint nil)
  "
  URL Stuff
  ----------
  [_g_] browse original              [_r_] read later
  [_G_] in-article browse original   [_R_] in-article read later

  Common but seldom used
  ----------------------
  [_f_] forward mail                 [_b_] show mail in browser
  [_o_] view attachment externally   [_h_] toggle verbose headers
  [_s_] save attachments

  Replying
  --------
  [_w_]: Mail -- Wide reply          [_W_]: Mail -- Wide reply w/ original
"
  ("g" fyi-article-browse-original)
  ("r" fyi-article-read-later)
  ("G" (gnus-article-urls-action #'browse-url))
  ("R" (gnus-article-urls-action #'fyi-capture-read-later #'fyi-article-subject))
  ("f" gnus-summary-mail-forward)
  ("o" gnus-mime-view-part-externally)
  ("s" gnus-mime-save-part)
  ("b" gnus-article-browse-html-article)
  ("h" gnus-summary-verbose-headers)
  ("w" gnus-summary-wide-reply)
  ("W" gnus-summary-wide-reply-with-original))

(define-key gnus-summary-mode-map (kbd "C-c C-.") 'hydra-gnus/body)
(define-key gnus-article-mode-map (kbd "C-c C-.") 'hydra-gnus/body)

;;; enable hl-line
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;;; Message hydra
(define-key
  message-mode-map
  (kbd "C-c C-.")
  (defhydra hydra-message (:color blue :hint nil)
    "
  Goto                                       Actions                          PGP/MIME
  -------------------------------------------------------------------------------------
  _t_: 'To' header [C-c C-f C-t]             _a_: Insert 'Mail-Follow-To'     _i_: Sign message
  _o_: 'From' header [C-c C-f C-o]           _S_: Change current subject      _e_: Encrypt message
  _b_: 'Bcc' header [C-c C-f C-b]            _w_: Insert signature            _p_: Preview
  _c_: 'Cc' header [C-c C-f C-c]             _x_: Cross-post
  _s_: 'Subject' header [C-c C-f C-s]        _A_: Attach file
  _r_: 'Reply-To' header [C-c C-f C-r]       _W_: Act as wide reply
  _f_: 'Followup-To' header [C-c C-f C-f]
  _n_: 'Newsgroups' header [C-c C-f C-n]
"
    ("t" message-goto-to)
    ("o" message-goto-from)
    ("b" message-goto-bcc)
    ("c" message-goto-cc)
    ("s" message-goto-subject)
    ("r" message-goto-reply-to)
    ("f" message-goto-followup-to)
    ("n" message-goto-newsgroups)
    ("a" message-generate-unsubscribed-mail-followup-to)
    ("S" message-change-subject)
    ("w" message-insert-signature)
    ("x" message-cross-post-followup-to)
    ("A" mml-attach-file)
    ("i" mml-secure-message-sign-pgpmime)
    ("e" mml-secure-message-encrypt-pgpmime)
    ("p" mml-preview)
    ("W" message-insert-wide-reply)))

;;; Find mail by message-id
;;; http://www.emacswiki.org/emacs/FindMailByMessageId
(defun gnus-goto-article (group message-id)
  (activate-gnus)
  (gnus-summary-read-group group 15 t)
  (let ((nnir-imap-default-search-key "imap"))
    (gnus-summary-refer-article message-id)))

;;; scoring
(setq gnus-score-default-duration 'p
      gnus-score-expiry-days 30
      gnus-use-adaptive-scoring '(line)
      gnus-score-interactive-default-score 10
      gnus-summary-mark-below -10
      gnus-summary-expunge-below -10
      gnus-thread-expunge-below -100
      gnus-summary-default-high-score 50
      gnus-decay-score "\\.ADAPT\\'")

(setq gnus-default-adaptive-score-alist
      '((gnus-forwarded-mark (subject 2) (from 2))
        (gnus-replied-mark (subject 5) (from 5))
        (gnus-read-mark (subject 1))
        (gnus-del-mark (subject -1))
        (gnus-killed-mark (subject -11))
        (gnus-catchup-mark (subject -11))
        (gnus-expirable-mark (subject -100))))

(add-hook 'message-sent-hook #'gnus-score-followup-article)
(add-hook 'message-sent-hook #'gnus-score-followup-thread)

(provide 'init-gnus)
