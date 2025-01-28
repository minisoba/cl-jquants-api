(in-package :cl-jquants-api)

(defclass trading-calendar (jquants-object)
  ((date :accessor date-of :initform 0)
   (holiday-division :accessor holiday-division-of :initform nil)))

(defmethod complete-object-update ((obj trading-calendar))
  (setf (holiday-division-of obj) (parse-integer (holiday-division-of obj))))
