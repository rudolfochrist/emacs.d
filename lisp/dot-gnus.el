(require 'gnus)
(require 'nnir)
(require 'org)
(require 'hydra)
(require 'bind-key)
(require 'bbdb)
(require 'bbdb-gnus)
(require 'bbdb-message)
(require 'bind-key)

;;; see http://blog.binchen.org/posts/notes-on-using-gnus.html
;;; Settings highly influenced by John Wiegley (https://github.com/jwiegley/dot-emacs/)

;;; me
(setq user-full-name "Sebastian Christ"
      user-mail-address "rudolfo.christ@gmail.com")

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
      gnus-topic-display-empty-topics nil)

;;; starting/activating errors
(defun activate-gnus ()
  "Start Gnus when not already running."
  (unless (get-buffer "*Group*")
    (gnus)))

(defun start-gnus (other-frame)
  "Opens Gnus unless Gnus' already running."
  (interactive "P")
  (let ((gnus-buffer (get-buffer "*Group*")))
    (if gnus-buffer
        (switch-to-buffer gnus-buffer)
      (if other-frame
          (gnus-other-frame)
        (gnus)))))

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
                      (nnimap-inbox "INBOX")
                      (nnimap-split-methods default)
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

(defun my-gnus-multi-tab ()
  "bbdb mail complete in message header. Yasnippet expand in message body."
  (interactive)
  (if (message-in-body-p)
      (yas-expand)
    (bbdb-complete-mail)))

;;; message mode hook
(defun my-message-mode-hook ()
  (message-setup-fill-variables)
  (ispell-change-dictionary "german8")
  (yas-minor-mode 1)
  (bbdb-mail-aliases)
  (local-set-key (kbd "TAB") 'my-gnus-multi-tab)
  (turn-on-orgtbl)
  (turn-on-orgstruct++))
(add-hook 'message-mode-hook #'my-message-mode-hook)

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
         (display . all)
         (gnus-article-sort-functions '(gnus-article-sort-by-most-recent-date))
         (gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date)))
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

;;; sending mail
;;; this need gnutls to be installed
(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smptmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtp-auth-crednetials (expand-file-name "~/.authinfo.gpg")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtp-debug-info t)

;;; wash GWENE
(defun my-gwene-wash-html ()
  (when (string-prefix-p "gwene" gnus-newsgroup-name)
    (gnus-article-wash-html)))
(add-hook 'gnus-article-prepare-hook 'my-gwene-wash-html)

;;; article highlighting
(add-hook 'gnus-article-mode-hook #'gnus-article-highlight)

;;; Lesser used article features in a hydra
(defun my-article-get-header (header &optional skip-bounds)
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

(defun my-article-archived-at ()
  "Return archived-at header of article in the current `gnus-article-buffer'."
  (my-article-get-header 'Archived-at t))

(defun my-article-message-id ()
  "Return the article's Message-ID."
  (my-article-get-header 'Message-ID))

(defun my-article-browse-original ()
  "Open current article in the browser."
  (interactive)
  (browse-url (or (my-article-archived-at)
                  (my-article-message-id))))

(defhydra gnus-hydra (:color blue :hint nil)
  "
  Goto
  ----------
  [_g_] browse original             

  Common but seldom used
  ----------------------
  [_f_] forward mail                 [_b_] show mail in browser
  [_o_] view attachment externally   [_h_] toggle verbose headers
  [_s_] save attachments

  Replying
  --------
  [_w_]: Mail -- Wide reply          [_W_]: Mail -- Wide reply w/ original
"
  ("g" my-article-browse-original)
  ("f" gnus-summary-mail-forward)
  ("o" gnus-mime-view-part-externally)
  ("s" gnus-mime-save-part)
  ("b" gnus-article-browse-html-article)
  ("h" gnus-summary-verbose-headers)
  ("w" gnus-summary-wide-reply)
  ("W" gnus-summary-wide-reply-with-original))

(bind-key "C-c C-." #'gnus-hydra/body gnus-summary-mode-map)
(bind-key "C-c C-." #'gnus-hydra/body gnus-article-mode-map)

;;; enable hl-line
(add-hook 'gnus-group-mode-hook 'hl-line-mode)
(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;;; Message hydra
(defhydra message-hydra (:color blue :hint nil)
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
  ("W" message-insert-wide-reply))

(bind-key "C-c C-." #'message-hydra/body message-mode-map)

;;; Find mail by message-id
;;; http://www.emacswiki.org/emacs/FindMailByMessageId
(defun gnus-goto-article (group message-id)
  (activate-gnus)
  (when (string-match "gmail" group)
    (setq group "nnimap+gmail:[Google Mail]/All Mail"))
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

;;; signature
(setq message-signature t
      message-signature-file "~/.signatures/private.sig")

;;; citation
(require 'supercite)

(setq message-default-headers "X-Attribution: SRC"
      sc-auto-fill-region-p nil
      sc-preferred-attribution-list
      '("x-attribution" "initials" "firstname" "lastname"))

(add-hook 'mail-citation-hook #'sc-cite-original)

;;; splitting mail
(setq nnmail-split-methods
      '(("list.lisp-hug" "To:.*lisp-hug@lispworks.com.*")
        ("list.lisp-hug" "Cc:.*lisp-hug@lispworks.com.*")
        ("list.elsconf" "To:.*elsconf@european-lisp-symposium.org.*")
        ("list.elsconf" "Cc:.*elsconf@european-lisp-symposium.org.*")
        ("list.elsconf" "To:.*elsconf@lrde.epita.fr.*")
        ("list.elsconf" "Cc:.*elsconf@lrde.epita.fr.*")
        ("mail.inbox" "")))

;;; attachment reminder
;;; see http://mbork.pl/2016-02-06_An_attachment_reminder_in_mu4e
(defun my-message-attachment-present-p ()
  "Return t if an attachment is found in the current message."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "<#part" nil t) t))))

(defcustom my-message-attachment-intent-re
  (regexp-opt '("I attach"
		"I have attached"
		"I've attached"
		"I have included"
		"I've included"
		"see the attached"
		"see the attachment"
		"attached file"
                "siehe im Anhang"
                "im Anhang"
                "Anhang"
                "anbei"))
  "A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

(defcustom my-message-attachment-reminder
  "Are you sure you want to send this message without any attachment? "
  "The default question asked when trying to send a message
containing `my-message-attachment-intent-re' without an
actual attachment.")

(defun my-message-warn-if-no-attachments ()
  "Ask the user if s?he wants to send the message even though
there are no attachments."
  (when (and (save-excursion
	       (save-restriction
		 (widen)
		 (goto-char (point-min))
		 (re-search-forward my-message-attachment-intent-re nil t)))
	     (not (my-message-attachment-present-p)))
    (unless (y-or-n-p my-message-attachment-reminder)
      (keyboard-quit))))

(add-hook 'message-send-hook #'my-message-warn-if-no-attachments)

;;; Add mail to BBDB
;;; https://emacs.stackexchange.com/a/10434
(defun gnus-bbdb-snarf-sender ()
  (interactive)
  (gnus-with-article-buffer
    (let ((from (mail-fetch-field "from")))
      (bbdb-snarf from 'mail))))

(bind-key "C-c C-c" #'gnus-bbdb-snarf-sender gnus-summary-mode-map)
(bind-key "C-c C-c" #'gnus-bbdb-snarf-sender gnus-article-mode-map)

(provide 'init-gnus)
