;;; indentation-rules.el --- Common Lisp Indentation Rule for Custom Macros

;;; Commentary:
;; Some custom macros have weird indentation in Emacs.  Load this to fix it.
;; Perhaps I should write some functions that make this more convenient

;;; Code:

;;; fiveam
(put 'test 'lisp-indent-function
     (get 'prog1 'lisp-indent-function))


(provide 'indentation-rules)

;;; indentation-rules.el ends here
