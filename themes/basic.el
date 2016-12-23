;;; general mode-line customizations

(defun set-faces-attribute (faces &rest args)
  "Set ARGS on multiple faces"
  (dolist (face faces)
    (apply 'set-face-attribute face nil args)))

;;; color, colors, colors
(defvar fyi-modeline-modified-bg "#AB4642"
  "The mode-line color when buffer is unsaved.")

(defvar fyi-modeline-default-bg "#7CAFC2"
  "The initial/default mode-line color.")

(defvar fyi-modeline-inactive-bg "#E8E8E8"
  "The inactive mode-line color.")

(defvar fyi-modeline-fg "#181818"
  "The mode-line foreground color. If `fyi-modeline-collapsed-p'.")

(defvar fyi-modeline-collapsed-height 20
  "The height of the mode-line in it's collapsed state.")

(defvar-local fyi-modeline-active-bg fyi-modeline-default-bg
  "The current background color.")

(defun fyi-modeline-collapsed-p ()
  "True if mode-line has a height of `fyi-modeline-collapsed-height'."
  (= fyi-modeline-collapsed-height
     (face-attribute 'mode-line :height nil 'default)))

(defun fyi-toggle-modeline ()
  "Expands/collapses the mode-line."
  (interactive)
  (setq face-remapping-alist (assq-delete-all 'mode-line face-remapping-alist))
  (cond
    ((fyi-modeline-collapsed-p)
     (set-faces-attribute '(mode-line mode-line-inactive)
                          :height (face-attribute 'default :height)
                          :foreground fyi-modeline-fg)
     (face-remap-add-relative 'mode-line `((:background ,fyi-modeline-active-bg)
                                           (:foreground ,fyi-modeline-fg)))
     (set-face-background 'mode-line-inactive fyi-modeline-inactive-bg))
    (t
     (set-faces-attribute '(mode-line mode-line-inactive)
                          :height fyi-modeline-collapsed-height)
     (face-remap-add-relative 'mode-line
                              `((:background ,fyi-modeline-active-bg)
                                (:foreground ,fyi-modeline-active-bg)))
     (set-face-attribute 'mode-line-inactive nil
                         :foreground fyi-modeline-inactive-bg
                         :background fyi-modeline-inactive-bg))))

(defun fyi-modeline-redraw (&rest _ignored)
  (setq face-remapping-alist (assq-delete-all 'mode-line face-remapping-alist))
  (face-remap-add-relative 'mode-line `((:background ,fyi-modeline-active-bg)))
  (when (fyi-modeline-collapsed-p)
    (face-remap-add-relative 'mode-line `((:foreground ,fyi-modeline-active-bg)))))

(defun fyi-modeline-mark-modified ()
  (setq fyi-modeline-active-bg fyi-modeline-modified-bg)
  (fyi-modeline-redraw))

(defun fyi-modeline-mark-saved (&rest _ignored)
  (setq fyi-modeline-active-bg fyi-modeline-default-bg)
  (fyi-modeline-redraw))

(defun enable-stateful-mode-line ()
  (interactive)
  (global-set-key (kbd "C-x tt") 'fyi-toggle-modeline)
  (add-hook 'after-save-hook 'fyi-modeline-mark-saved)
  (add-hook 'after-revert-hook 'fyi-modeline-mark-saved)
  (add-hook 'gnus-after-exiting-gnus-hook 'fyi-modeline-redraw)
  (add-hook 'first-change-hook 'fyi-modeline-mark-modified)
  (advice-add 'select-window :after 'fyi-modeline-redraw)
  (advice-add 'undo :after (lambda (&rest _ignored)
                             (unless (buffer-modified-p)
                               (setq fyi-modeline-active-bg
                                     fyi-modeline-default-bg)
                               (fyi-modeline-redraw)))))

;;; basic-theme plus customizations
(require-package 'basic-theme)
(load-theme 'basic t)

;;; color-scheme -> https://github.com/chriskempson/base16
;;; my basic-theme adjustments
(custom-set-faces
 ;; basic faces
 '(italic ((t (:slant italic))))
 '(underline ((t :underline t)))
 '(isearch ((t :background "#86C1B9")))
 '(highlight ((t :background "#A1B56C")))
 ;; font locking
 '(font-lock-comment-delimiter-face ((t :foreground "#585858")))
 '(font-lock-comment-face ((t :foreground "#585858")))
 '(font-lock-function-name-face ((t :bold t)))
 '(font-lock-builtin-face ((t :bold t)))
 '(font-lock-keyword-face ((t :bold t)))
 '(font-lock-doc-face ((t :slant italic)))
 '(font-lock-string-face ((t :slant italic)))
 ;; info
 '(info-xref ((t :foreground "#7CAFC2")))
 ;; org-mode
 '(org-link ((t (:underline t :bold t :foreground "#AB4642"))))
 '(org-date-selected ((t (:background "#AB4642" :foreground "#F8F8F8"))))
 '(org-warning ((t (:foreground "#AB4642" :bold t))))
 '(org-verbatim ((t (:foreground "#7CAFC2"))))
 '(org-code ((t (:foreground "#7CAFC2"))))
 '(org-block ((t (:background "#D8D8D8"))))
 '(org-level-1 ((t :foreground "#AB4642" :bold t)))
 '(org-level-2 ((t :foreground "#7CAFC2" :bold t)))
 '(org-level-3 ((t :foreground "#7CAFC2" :bold t)))
 '(org-level-4 ((t :foreground "#7CAFC2" :bold t)))
 '(org-level-5 ((t :foreground "#7CAFC2" :bold t)))
 '(org-level-6 ((t :foreground "#7CAFC2" :bold t)))
 '(org-level-7 ((t :foreground "#7CAFC2" :bold t)))
 '(org-level-8 ((t :foreground "#7CAFC2" :bold t)))
 ;; linum-realtive
 '(linum-relative-current-face ((t :foreground "#A16946")))
 ;; magit
 '(magit-branch-current ((t :foreground "#7CAFC2"))))

