;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:__PROJECT-NAME__/main
  (:use :cl)
  (:export
   #:main
   #:reload))

(in-package #:__PROJECT-NAME__/main)

(defun reload ()
  ;; reload system and/or config
  )

(defun handle-interrupt (signo info-sap context-sap)
  (declare (ignore info-sap context-sap))
  ;; cleanup/finalize
  (uiop:quit
   (cond
     ((eql signo sb-unix:sigterm) 0)
     ((eql signo sb-unix:sigint) 130)
     (t 1))))

(defun handle-sighup (signo info-sap context-sap)
  (declare (ignore signo info-sap context-sap))
  (reload))

(defun main (&optional args)
  (declare (ignorable args))
  (sb-sys:enable-interrupt sb-unix:sigint #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sigterm #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sighup #'handle-sighup)
  ;; run program
  )
