(in-package :cl-user)

(defpackage :cl-jquants-api.test
  (:use :cl :alexandria :fiveam :cl-jquants-api)
  (:import-from :cl-jquants-api #:send-request)
  (:export
   #:run-tests
   #:cl-jquants-api-tests))

(in-package :cl-jquants-api.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite cl-jquants-api-tests
    :description "All cl-jquants-api tests")
  #+lispworks
  (progn
    (lw:set-default-character-element-type 'cl:character)
    (setf system:*specific-valid-file-encodings* '(:latin-1 :utf-8))
    (setf system:*default-valid-file-encoding* :utf-8)))
