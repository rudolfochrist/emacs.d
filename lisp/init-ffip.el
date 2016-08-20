(require-package 'find-file-in-project)

;;; prune lisp files
(setq ffip-prune-patterns
      (append ffip-prune-patterns
              (list "*.fasl" "*.abcl" "*.dx32fsl" "*.dx64fsl")))

(defun ffip-apply-project-buffers (fn)
  "Applies FN to each buffer of the current project, denoted by `ffip-project-root'."
  (let ((buffers (remove-if-not (lambda (buffer)
                                  (when (and (buffer-file-name buffer)
                                             (ffip-project-root))
                                    (string-prefix-p (expand-file-name (ffip-project-root))
                                                     (buffer-file-name buffer))))
                                (buffer-list))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (funcall fn)))))

(defun ffip-save-project-buffers ()
  (interactive)
  (ffip-apply-project-buffers #'save-buffer)
  (message "Saved buffers."))

(defun ffip-kill-project-buffers ()
  (interactive)
  (ffip-apply-project-buffers #'kill-buffer)
  (message "Killed buffers."))

(defun ffip-revert-project-buffers ()
  (interactive)
  (ffip-apply-project-buffers
   (lambda ()
     (revert-buffer nil t)))
  (message "Reverted buffers."))

(defvar ffip-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'find-file-in-project)
    (define-key map (kbd "s") #'ffip-save-project-buffers)
    (define-key map (kbd "k") #'ffip-kill-project-buffers)
    (define-key map (kbd "r") #'ffip-revert-project-buffers)
    map))
(define-key (current-global-map) (kbd "C-c p") ffip-command-map)

(provide 'init-ffip)
