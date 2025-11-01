;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(require :asdf)
(in-package :cl-user)

(unless (uiop:file-exists-p "doc/__PROJECT-NAME__.texi")
  (error "Cannot build documentation: doc/__PROJECT-NAME__.texi is missing."))

(asdf:load-system "__PROJECT-NAME__/doc")
(sb-texinfo:document-packages (list :sb-async) "__PROJECT-NAME__"
                              :output-file "doc/dict.texi"
                              :standalone nil
                              :write-backmatter nil
                              :write-menu nil
                              :exclude-node t)
(uiop:delete-file-if-exists "__PROJECT-NAME__.info")
(uiop:delete-file-if-exists "__PROJECT-NAME__.html")
(uiop:run-program (list "makeinfo" "-o" "." "doc/__PROJECT-NAME__.texi"))
(uiop:run-program (list "makeinfo" "--html" "--no-split" "-o" "." "doc/__PROJECT-NAME__.texi"))
