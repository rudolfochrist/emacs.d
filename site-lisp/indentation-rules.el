;;; indentation-rules.el --- Common Lisp Indentation Rule for Custom Macros

;;; Commentary:
;; Some custom macros have weird indentation in Emacs.  Load this to fix it.
;; Perhaps I should write some functions that make this more convenient

;;; Code:

(require 'slime)

;;; https://github.com/eudoxia0/cl-yaml
(put 'with-emitter-to-string 'slime-common-lisp-indent-function
     (get 'when 'slime-common-lisp-indent-function))

(put 'with-emitter-to-stream 'slime-common-lisp-indent-function
     (get 'when 'slime-common-lisp-indent-function))

;;; asdf
(put 'defsystem 'slime-common-lisp-indent-function
     (get 'defun 'slime-common-lisp-indent-function))

;;; jzon
(put 'with-writer* 'slime-common-lisp-indent-function
     (get 'destructuring-bind 'slime-common-lisp-indent-function))
(put 'with-writer 'slime-common-lisp-indent-function
     (get 'destructuring-bind 'slime-common-lisp-indent-function))

;;; fiveam
(put 'test 'slime-common-lisp-indent-function
     (get 'prog1 'slime-common-lisp-indent-function))

(provide 'indentation-rules)

;;; indentation-rules.el ends here
