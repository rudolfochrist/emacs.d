;;; indentation-rules.el --- Common Lisp Indentation Rule for Custom Macros

;;; Commentary:
;; Some custom macros have weird indentation in Emacs.  Load this to fix it.
;; Perhaps I should write some functions that make this more convenient

;;; Code:

(require 'slime)
(require 'slime-cl-indent)

;;; https://github.com/eugeneia/maxpc
(put '=destructure 'common-lisp-indent-function
     (get 'destructuring-bind 'common-lisp-indent-function))

(put 'handle-command-line 'common-lisp-indent-function
     (get 'defun 'common-lisp-indent-function))

(put 'import 'common-lisp-indent-function
     (get 'defun 'common-lisp-indent-function))

(put 'if-let 'common-lisp-indent-function
     (get 'let 'common-lisp-indent-function))

;;; https://docs.stevelosh.com/adopt
(put 'adopt:make-option 'common-lisp-indent-function
     (get 'when 'common-lisp-indent-function))

;;; https://github.com/eudoxia0/cl-yaml
(put 'with-emitter-to-string 'common-lisp-indent-function
     (get 'when 'common-lisp-indent-function))

(put 'with-emitter-to-stream 'common-lisp-indent-function
     (get 'when 'common-lisp-indent-function))

;;; postmoderb
(put ':create-table 'common-lisp-indent-function
     (get 'defstruct 'common-lisp-indent-function))

(provide 'indentation-rules)

;;; indentation-rules.el ends here
