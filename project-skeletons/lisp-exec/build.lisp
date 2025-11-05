;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(require 'asdf)
(require 'uiop)
#+sbcl (require 'sb-aclrepl)

(in-package #:cl-user)

(asdf:initialize-source-registry
 `(:source-registry
   :ignore-inherited-configuration
   (:tree ,(uiop:getcwd))))

(with-compilation-unit (:policy '(optimize (safety 1) (debug 1) (compilation-speed 3) (space 3) (speed 3)) :override t)
  (asdf:make "__PROJECT-NAME__/main"))

(uiop:quit)
