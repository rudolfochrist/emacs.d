(setq gnus-select-method '(nnml "")
      gnus-home-score-file (expand-file-name "~/.emacs.d/total-scoring.SCORE")
      gnus-secondary-select-methods '((nntp "news.gmane.org"
                                       (nntp-open-connection-function nntp-open-tls-stream)
                                       (nntp-port-number 563)
                                       (nntp-address "news.gmane.org"))
                                      (nntp "news.eternal-september.org"
                                       (nntp-open-connection-function nntp-open-tls-stream)
                                       (nntp-port-number 563)
                                       (nntp-address "news.eternal-september.org")))
      message-directory (expand-file-name "~/gnus/mail/")
      gnus-directory (expand-file-name "~/gnus/news/")
      nnfolder-directory (expand-file-name "~/gnus/mail/archive"))

(global-set-key (kbd "<M-f11>") 'gnus)

(provide 'init-gnus)
