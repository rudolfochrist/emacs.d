;;; see http://mbork.pl/2016-02-06_An_attachment_reminder_in_mu4e

(defun fyi-message-attachment-present-p ()
  "Return t if an attachment is found in the current message."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward "<#part" nil t) t))))

(defcustom fyi-message-attachment-intent-re
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

(defcustom fyi-message-attachment-reminder
  "Are you sure you want to send this message without any attachment? "
  "The default question asked when trying to send a message
containing `fyi-message-attachment-intent-re' without an
actual attachment.")

(defun fyi-message-warn-if-no-attachments ()
  "Ask the user if s?he wants to send the message even though
there are no attachments."
  (when (and (save-excursion
	       (save-restriction
		 (widen)
		 (goto-char (point-min))
		 (re-search-forward fyi-message-attachment-intent-re nil t)))
	     (not (fyi-message-attachment-present-p)))
    (unless (y-or-n-p fyi-message-attachment-reminder)
      (keyboard-quit))))

(add-hook 'message-send-hook #'fyi-message-warn-if-no-attachments)

(provide 'init-attachment-reminder)
