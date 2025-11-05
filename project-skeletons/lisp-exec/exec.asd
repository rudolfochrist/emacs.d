
(defsystem "__PROJECT-NAME__/main"
  :build-operation program-op
  :build-pathname "__PROJECT-NAME__"
  :entry-point "__PROJECT-NAME__/main:main"
  :depends-on ("__PROJECT-NAME__")
  :components ((:file "main")))

#+sb-core-compression
(defmethod asdf:perform ((op asdf:image-op) (c (eql (asdf:find-system "__PROJECT-NAME__/main"))))
  (sb-ext:gc :full t)
  (uiop:dump-image (asdf:output-file op c) :executable t :compression t))
