(defun counsel-fyi-kb-search-function (string &rest unused)
  "Lookup STRING with index-cli"
  (if (< (length string) 3)
      (counsel-more-chars 3)
      (counsel--async-command
       (format "index-cli -i /Users/fyi/org -q \"%s\"" string))
      nil))

(defun search-kb (&optional initial-input)
  "Search KB"
  (interactive)
  (ivy-read "search KB: " 'counsel-fyi-kb-search-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action (lambda (x)
                      (when (string-match ".*\\[\\(.*\\)|\\([[:digit:]]+\\)\\]" x)
                        (let ((file-name (match-string 1 x))
                              (point (string-to-number (match-string 2 x))))
                          (find-file file-name)
                          (goto-char point)
                          (org-show-entry)
                          (show-children))))))



(provide 'init-search-kb)
