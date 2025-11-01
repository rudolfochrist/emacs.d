;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defsystem "__PROJECT-NAME__"
  :description "__DESCRIPTION__"
  :author "__USER-NAME__"
  :mailto "__USER-MAIL-ADDRESS__"
  :license "MPL-2.0"
  :homepage "https://codeberg.org/rudolfochrist/__PROJECT-NAME__"
  :version (:read-file-line "version")
  :depends-on ((:require "uiop"))
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "__PROJECT-NAME__/test"))))


(defsystem "__PROJECT-NAME__/test"
  :depends-on ((:require "uiop")
               "fiveam"
               "__PROJECT-NAME__")
  :pathname "t/"
  :components ((:file "__PROJECT-NAME__"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :fiveam :run! :__PROJECT-NAME__)
                      #+(not (or :swank :slynk))
                      (uiop:quit 1))))

(defsystem "__PROJECT-NAME__/doc"
  :if-feature :sbcl
  :depends-on ("sb-texinfo"
               "__PROJECT-NAME__"))
