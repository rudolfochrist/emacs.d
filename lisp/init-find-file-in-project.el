(require-package 'find-file-in-project)

(push "*.fasl" ffip-prune-patterns)
(push "*.abcl" ffip-prune-patterns)

(global-set-key (kbd "C-x C-g") 'find-file-in-project)
(global-set-key (kbd "C-x G") 'find-file-in-project-by-selected)

;;; see: https://github.com/bbatsov/projectile/blob/master/projectile.el#L953
(defun ffip-project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (and (not (string-prefix-p " " (buffer-name buffer)))
         (string-equal (file-remote-p default-directory)
                       (file-remote-p project-root))
         (not (string-match-p "^http\\(s\\)?://" default-directory))
         (string-prefix-p project-root (file-truename default-directory)))))

(defun ffip-project-buffers ()
  (let ((project-root (ffip-get-project-root-directory)))
    (remove-if-not (lambda (buffer)
                     (ffip-project-buffer-p buffer project-root))
                   (buffer-list))))

(defun ffip-save-project-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (when (buffer-file-name)
              (save-buffer))))
        (ffip-project-buffers)))

(global-set-key (kbd "C-x t s") #'ffip-save-project-buffers)

(provide 'init-find-file-in-project)
