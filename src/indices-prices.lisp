(in-package :cl-jquants-api)

(defclass indices-prices (jquants-object)
  ((code :accessor code-of :initform nil)
   (date :accessor date-of :initform 0)
   (open-price :accessor open-price-of :initform 0.0)
   (high-price :accessor high-price-of :initform 0.0)
   (low-price :accessor low-price-of :initform 0.0)
   (close-price :accessor close-price-of :initform 0.0)))

(defmethod complete-object-update ((obj indices-prices))
  (setf (code-of obj) (make-keyword (code-of obj))))
