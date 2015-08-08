(require-package 'bbdb)
(require-package 'bbdb-ext)
(require-package 'bbdb-vcard)
(require 'gnus)
(require 'nnir)

;;; see http://blog.binchen.org/posts/notes-on-using-gnus.html

;;; globals
(setq gnus-select-method '(nnml "")
      gnus-home-score-file (expand-file-name "~/.emacs.d/all.SCORE")
      gnus-use-cache t
      ;; fetch only part of the article. If possible.
      gnus-read-active-file 'some
      ;; don't keep message buffers around
      message-kill-buffer-on-exit t
      gnus-use-correct-string-widths nil
      mm-text-html-renderer 'gnus-w3m
      mm-inline-text-html-with-images t)

;;; display inlined images
(add-to-list 'mm-attachment-override-types "image/.*")

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
             '(nntp "nntp.aioe.org"
               (nntp-open-connection-function nntp-open-tls-stream)
               (nntp-port-number 563)
               (nntp-address "nntp.aioe.org")))

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
                               (fyi-configure-flyspell "german8")
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
(bbdb-initialize '(gnus mail message pgp anniv))

(defun fyi-gnus-archive-inbox ()
  (interactive)
  (mark-whole-buffer)
  (gnus-summary-delete-article))

;;; https://github.com/jwiegley/dot-emacs/blob/master/dot-gnus.el#L530
(defun fyi-gnus-article-get-urls-region (min max)
  "Return a list of urls found in the region between MIN and MAX"
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

;;; https://github.com/jwiegley/dot-emacs/blob/master/dot-gnus.el#L544
(defun fyi-gnus-article-get-current-urls ()
  "Return a list of the urls found in the current `gnus-article-buffer'"
  (let (url-list)
    (with-current-buffer gnus-article-buffer
      (setq url-list
            (fyi-gnus-article-get-urls-region (point-min) (point-max))))
    url-list))

;;; https://github.com/jwiegley/dot-emacs/blob/master/dot-gnus.el#L552
(defun fyi-gnus-article-browse-urls ()
  "Visit a URL from the `gnus-article-buffer' by showing a
buffer with the list of URLs found with the `gnus-button-url-regexp'."
  (interactive)
  (gnus-configure-windows 'article)
  (gnus-summary-select-article nil nil 'pseudo)
  (let ((temp-buffer (generate-new-buffer " *Article URLS*"))
        (urls (fyi-gnus-article-get-current-urls))
        (this-window (selected-window))
        (browse-window (get-buffer-window gnus-article-buffer))
        (count 0))
    (save-excursion
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
          (kill-buffer temp-buffer)))
      (if browse-window
          (progn (select-window browse-window)
                 (browse-url (nth count urls)))))
    (select-window this-window)))

(defun fyi-gnus-keybindings ()
  (define-key gnus-summary-mode-map (kbd "C-c C-a") 'fyi-gnus-archive-inbox)
  (define-key gnus-summary-mode-map (kbd "C-c C-u") 'fyi-gnus-article-browse-urls))
(add-hook 'gnus-summary-mode-hook 'fyi-gnus-keybindings)

(global-set-key (kbd "<f11>") 'gnus)

(provide 'init-gnus)
