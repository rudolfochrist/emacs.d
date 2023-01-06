;;; t/tests.lisp

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:__PROJECT-NAME__/test
  (:use :cl :1am))

(in-package #:__PROJECT-NAME__/test)

(defvar *tests* nil)

(defmacro deftest (name &body body)
  `(progn
     (test ,name
       ,@body)
     (pushnew ',name *tests*)
     ',name))

(defun run-tests (&optional (tests *tests*))
  (1am:run tests))

;;; Test cases



