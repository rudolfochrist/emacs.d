;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defsystem "__PROJECT-NAME__"
  :author "__USER-NAME__ <__USER-MAIL-ADDRESS__>"
  :maintainer "__USER-NAME__ <__USER-MAIL-ADDRESS__>"
  :mailto "__USER-MAIL-ADDRESS__"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/__PROJECT-NAME__"
  :bug-tracker "https://github.com/rudolfochrist/__PROJECT-NAME__/issues"
  :source-control (:git "https://github.com/rudolfochrist/__PROJECT-NAME__.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop"))
  :components ()
  :description "__DESCRIPTION__"
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :in-order-to ((test-op (test-op "__PROJECT-NAME__/test"))))


(defsystem "__PROJECT-NAME__/test"
  :description "Tests for __PROJECT-NAME__"
  :depends-on ((:require "uiop")
               "fiveam"
               "fiveam-matchers"
               "__PROJECT-NAME__")
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :fiveam :run! :__PROJECT-NAME__/test)
                      #+(not (or :swank :slynk))
                      (uiop:quit 1))))


