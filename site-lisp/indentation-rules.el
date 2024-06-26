;;; indentation-rules.el --- Common Lisp Indentation Rule for Custom Macros

;;; Commentary:
;; Some custom macros have weird indentation in Emacs.  Load this to fix it.
;; Perhaps I should write some functions that make this more convenient

;;; Code:

(require 'sly)

;;; https://github.com/eudoxia0/cl-yaml
(put 'with-emitter-to-string 'sly-common-lisp-indent-function
     (get 'when 'sly-common-lisp-indent-function))

(put 'with-emitter-to-stream 'sly-common-lisp-indent-function
     (get 'when 'sly-common-lisp-indent-function))

;;; asdf
(put 'defsystem 'sly-common-lisp-indent-function
     (get 'defun 'sly-common-lisp-indent-function))

;;; jzon
(put 'with-writer* 'sly-common-lisp-indent-function
     (get 'destructuring-bind 'sly-common-lisp-indent-function))
(put 'with-writer 'sly-common-lisp-indent-function
     (get 'destructuring-bind 'sly-common-lisp-indent-function))

(provide 'indentation-rules)

;;; indentation-rules.el ends here
