
(defsystem "__PROJECT-NAME__/image"
  :build-operation image-op
  :build-pathname "__PROJECT-NAME__"
  :depends-on ("__PROJECT-NAME__")
  :components ((:file "main")))

(defmethod asdf:perform ((op asdf:image-op) (c (eql (asdf:find-system "__PROJECT-NAME__/main"))))
  (sb-ext:gc :full t))
