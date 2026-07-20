;;; yarg.el --- ripgrep interface              -*- lexical-binding: t; -*-

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: tools, processes
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 50-loc minimal Emacs interface to ripgrep (rg), modelled after
;; https://github.com/leoliu/ack-el.
;;
;; Say you bind C-c s to `yarg':
;;   C-c s           Search symbol at point from project root immediately.
;;   C-u C-c s       Same, but edit the suggested rg command first.
;;   C-u C-u C-c s   Choose a directory and edit the rg command.
;;
;; The only user option is `yarg-switches'.

;;; Code:

(require 'compile)
(require 'ansi-color)
(require 'thingatpt)
(require 'project)

(defgroup yarg nil "Run ripgrep and display results." :group 'tools)

(defcustom yarg-switches
  "-S -. -g !.git -M 1500"
  "Extra `rg' switches after mandatory ones and before -e <pattern>."
  :type 'string)

(defun yarg--filter ()
  (let ((ansi-color-apply-face-function
         (lambda (b e f) (when f (ansi-color-apply-overlay-face b e f)
                           (put-text-property b e 'yarg t)))))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun yarg--col (endp)
  (let* ((b (match-end 0)) (e (pos-eol b)) (m (text-property-any b e 'yarg t)))
    (and m (- (if endp (next-single-property-change m 'yarg nil e) m) b))))

(defun yarg--cb () (yarg--col nil)) (defun yarg--ce () (yarg--col t))

(defconst yarg--re "^\\(?1:.+?\\)\\(?::\\)\\(?3:[1-9][0-9]*\\)\\(?::\\)\\(?4:[1-9][0-9]*\\)\\(?::\\)")
(defconst yarg-error-regexp-alist
  ;; rg --no-heading --column produces: file:line:col:content
  ;; Group 4 is the column number (always present with --column).
  `((,yarg--re 1 3 (yarg--cb . yarg--ce) nil 1 (4 compilation-column-face nil t))
    ("^Binary file \\(.+\\) matches$" 1 nil nil 0 1))
  "Compilation error-regexp-alist for rg --no-heading --column output.")

(define-compilation-mode yarg-mode "Yarg"
  "Compilation mode for ripgrep output."
  (setq-local compilation-disable-input t)
  (setq-local compilation-error-face 'compilation-info)
  (add-hook 'compilation-filter-hook #'yarg--filter nil t)
  (setq-local wgrep-line-file-regexp yarg--re)
  (ignore-errors (with-no-warnings (wgrep-setup-internal))))

(defvar yarg-history nil "Minibuffer history for `yarg'.")

;;;###autoload
(defun yarg (arg)
  "Run ripgrep from the project root, collecting output in `yarg-mode'.

With no prefix ARG: search for the symbol at point immediately.
With one \\[universal-argument]: pre-fill the symbol at point but allow
  editing the rg command before running.
With two \\[universal-argument]'s: choose the search directory, then
  edit the rg command.

The search pattern is quoted and passed after rg's -e flag so it
is always treated as a literal string unless the user edits it."
  (interactive "P")
  (let* ((numeric (prefix-numeric-value arg))
         (symbol (thing-at-point 'symbol t))
         (proj (project-current))
         (directory (if (>= numeric 16)
                        (read-directory-name "Search in: " nil nil t)
                      (if proj (project-root proj) default-directory)))
         (run-p (and symbol (< numeric 4)))
         (thing (if run-p (shell-quote-argument symbol) "''"))
         (cmd (format "rg --column --color always --no-heading %s -e %s"
                      yarg-switches thing))
         (command (if run-p cmd
                    (read-from-minibuffer
                     "Yarg: " (cons cmd (length cmd))
                     nil nil 'yarg-history))))
    (let ((default-directory (expand-file-name directory)))
      (compilation-start command 'yarg-mode))))

(provide 'yarg)
;;; yarg.el ends here
