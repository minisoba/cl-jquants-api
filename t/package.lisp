(in-package :cl-user)

(defpackage :cl-jquants-api.test
  (:use :cl :alexandria :fiveam :cl-jquants-api)
  (:import-from :cl-jquants-api #:send-request)
  (:export
   #:run-test))
