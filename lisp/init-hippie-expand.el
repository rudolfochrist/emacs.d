(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-list
        try-expand-line
        try-complete-file-name-partially
        try-complete-lisp-symbol-partially))

(global-set-key (kbd "M-/") 'hippie-expand)

(provide 'init-hippie-expand)
