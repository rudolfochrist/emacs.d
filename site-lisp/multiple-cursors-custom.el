;;; Code customizations and corrections for `multiple-cursors'.

(defun mc/execute-command-for-fake-cursor (cmd cursor)
  (let ((mc--executing-command-for-fake-cursor t)
        (id (overlay-get cursor 'mc-id))
        (annoying-arrows-mode nil)
        (smooth-scroll-margin 0))
    (mc/add-fake-cursor-to-undo-list
     (mc/pop-state-from-overlay cursor)
     (ignore-errors
       (mc/execute-command cmd)
       ;; (mc/create-fake-cursor-at-point id)
       ;; This line creates new fake cursors. Why?
       ;; This breaks `mc/insert-numbers' by creating for each fake
       ;; cursor another one and inserts a number for this cursor as
       ;; well. This results in funny numberings like this
       ;;
       ;;   1.jpg
       ;;   24.jpg
       ;;   35.jp
       ;;
       ;; No idea. Better turn it off. Since mc doesn't accept PR, we
       ;; use this code here.
       ))))
