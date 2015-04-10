(require-package 'engine-mode)

(engine-mode t)

(defengine duckduckgo
    "https://duckduckgo.com/?q=%s" "d")

(defengine github
    "https://github.com/search?ref=simplesearch&q=%s" "g")

(defengine stack-overflow
    "https://stackoverflow.com/search?q=%s" "s")

(provide 'init-engine-mode)
