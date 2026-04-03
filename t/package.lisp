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
      (let ((system-package (find-package :system)))
        (when system-package
          (let ((specific-encodings (find-symbol "*SPECIFIC-VALID-FILE-ENCODINGS*" system-package))
                (default-encoding (find-symbol "*DEFAULT-VALID-FILE-ENCODING*" system-package)))
            (when (and specific-encodings (boundp specific-encodings))
              (setf (symbol-value specific-encodings) '(:latin-1 :utf-8)))
            (when (and default-encoding (boundp default-encoding))
              (setf (symbol-value default-encoding) :utf-8)))))))
