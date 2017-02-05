(require-package 'slime)

;;; slime-setup requires this when the contrib is needed. 
(require-package 'slime-company :load-only t)




(defslime-repl-shortcut slime-repl-quicklisp-quickload
  ("quicklisp-quickload" "ql")
  (:handler (lambda (&rest systems)
              (interactive (list (slime-read-system-name)))
              (insert (format "(ql:quickload '%s)" systems))
              (slime-repl-send-input t)))
  (:one-liner "cl:quickload system"))


;;; show autodoc also on newline.
(defun fyi-slime-autodoc-newline ()
  (interactive)
  (if (eq major-mode 'slime-repl-mode)
      (slime-repl-newline-and-indent)
    (newline-and-indent))
  (let ((doc (slime-autodoc t)))
    (when doc
      (eldoc-message "%s" doc))))
(eldoc-add-command 'fyi-slime-autodoc-newline)

;;; switch to repl and back again
(defun fyi-slime-repl-switch ()
  (interactive)
  (cond
   ((eq major-mode 'slime-repl-mode)
    (other-window 1)
    (switch-to-buffer (first (fyi-mode-buffer-list 'lisp-mode))))
   (t
    (slime-switch-to-output-buffer))))

(defun fyi-open-lisp ()
  (interactive)
  (let ((repl-buffers (fyi-mode-buffer-list 'slime-repl-mode)))
    (cond
     ;; no REPL active. Start new.
     ((null repl-buffers)
      (call-interactively #'slime))
     ;; one REPL active. Switch to it.
     ((= 1 (length repl-buffers))
      (switch-to-buffer (first repl-buffers)))
     ;; ivy select buffer
     (t
      (ivy-read "Lisp REPL: " (mapcar #'buffer-name repl-buffers)
                :require-match t
                :action (lambda (buffer)
                          (switch-to-buffer buffer)))))))
(global-set-key (kbd "<f5>") #'fyi-open-lisp)

;;; file header
(defun fyi-slime-insert-header (cl-style coding)
  (interactive
   (cond
    ((not current-prefix-arg)
     (list nil nil))
    ((equal current-prefix-arg '(4))
     (list "modern" "utf-8-unix"))
    ((equal current-prefix-arg '(16))
     (list
      (completing-read "Style: " common-lisp-styles)
      (completing-read "Coding: " coding-system-list)))))
  (when (and cl-style coding)
    (insert (format ";;; -*- mode: Lisp; common-lisp-style: %s; slime-coding: %s -*-\n;;;\n"
                    cl-style coding)))
  (let ((file-name (buffer-file-name)))
    (insert ";;; " (file-name-base file-name) (file-name-extension file-name t))))

;;; Inspect last expresssion
(defun slime-repl-inspect-last-expression ()
  "Inspects the last expression."
  (interactive)
  (slime-repl-inspect "*"))

;;; docstring in minibuffer
(defun slime-documentation-in-minibuffer (symbol)
  (interactive
   (let ((s-a-p (slime-symbol-at-point)))
     (cond
      ((or current-prefix-arg
           (not s-a-p))
       (list (slime-read-symbol-name "Symbol: ")))
      (t
       (list s-a-p)))))
  (let ((doc (slime-eval `(swank:documentation-symbol ,symbol))))
    ;; calculate if it fits into the echo areas
    ;; see: `display-message-or-buffer'
    (if (<= (count-lines-string doc)
            (typecase max-mini-window-height
              (integer max-mini-window-height)
              (float (* (frame-height)
                        max-mini-window-height))))
        (message "%s" doc)
      (slime-documentation symbol))))

;;; location asd or package files
(require 'find-file-in-project)

(defun fyi-find-current-asd-file ()
  (interactive)
  (let* ((dir (or (ffip-get-project-root-directory)
                  default-directory))
         (asd (concat dir 
                      (file-name-base (directory-file-name dir))
                      ".asd")))
    (find-file asd)))

(defun fyi-find-current-package-file ()
  (interactive)
  (let ((ffip-project-root (or (ffip-get-project-root-directory)
                               default-directory))
        file)
    ;; using setq to have access to ffip-project-root binding
    (setq file (cdar (ffip-project-search "package.lisp" nil)))
    (when file
      (find-file file))))

(defun fyi-slime-keybindings ()
  (define-key slime-mode-map (kbd "RET") #'fyi-slime-autodoc-newline)
  (define-key slime-mode-map (kbd "C-c C-z") #'fyi-slime-repl-switch)
  (define-key slime-mode-map (kbd "C-c C-d i") #'fyi-slime-insert-header)
  (define-key slime-mode-map (kbd "C-c C-d s") #'slime-documentation-in-minibuffer)
  (define-key slime-mode-map (kbd "C-c C-d ,") #'fyi-find-current-asd-file)
  (define-key slime-mode-map (kbd "C-c C-d .") #'fyi-find-current-package-file))

(add-hook 'slime-mode-hook 'fyi-slime-keybindings)

(defun fyi-slime-repl-keybindings ()
  (define-key slime-repl-mode-map (kbd "C-l") #'slime-repl-clear-buffer)
  (define-key slime-repl-mode-map (kbd "SPC") #'slime-autodoc-space)
  (define-key slime-repl-mode-map (kbd "C-j") #'fyi-slime-autodoc-newline)
  (define-key slime-repl-mode-map (kbd "C-c C-z") #'fyi-slime-repl-switch)
  (define-key slime-repl-mode-map (kbd "C-c O") #'slime-repl-inspect-last-expression)
  (define-key slime-repl-mode-map (kbd "C-c C-d s") #'slime-documentation-in-minibuffer))

(add-hook 'slime-repl-mode-hook 'fyi-slime-repl-keybindings)

(provide 'init-slime)
