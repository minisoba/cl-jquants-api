(asdf:defsystem :cl-jquants-api-test
    :author "Y. IGUCHI"
    :license "MIT"
    :description "Unit tests for cl-jquants-api"
    :serial t
    :depends-on (:cl-jquants-api
                 :alexandria
                 :cl-ppcre
                 :fiveam
                 :local-time
                 :parse-number
                 :uiop
                 :yason)
    :components ((:file "t/package")
                 (:file "t/override")
                 (:file "t/api"))
    :perform (asdf:test-op
              (o s)
              (uiop:symbol-call
               :cl-jquants-api.test '#:run-test)))
