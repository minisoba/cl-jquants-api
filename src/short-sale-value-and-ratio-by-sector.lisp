(in-package :cl-jquants-api)

(defclass short-sale-value-and-ratio-by-sector (jquants-object)
  ((date :accessor date-of :initform 0)
   (sector33-code :accessor sector33-code-of :initform nil)
   (selling-excluding-short-selling-turnover-value :accessor selling-excluding-short-selling-turnover-value-of :initform 0.0)
   (short-selling-with-restrictions-turnover-value :accessor short-selling-with-restrictions-turnover-value-of :initform 0.0)
   (short-selling-without-restrictions-turnover-value :accessor short-selling-without-restrictions-turnover-value-of :initform 0.0)))

(defmethod complete-object-update :after ((obj short-sale-value-and-ratio-by-sector))
  (setf (sector33-code-of obj) (make-keyword (sector33-code-of obj))))
