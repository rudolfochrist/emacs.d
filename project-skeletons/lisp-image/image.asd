
(defsystem "__PROJECT-NAME__/image"
  :build-operation image-op
  :build-pathname "__PROJECT-NAME__"
  :depends-on ("__PROJECT-NAME__")
  :components ((:file "main")))
