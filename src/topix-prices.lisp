(in-package :cl-jquants-api)

(defclass topix-prices (jquants-object)
  ((date :accessor date-of :initform 0)
   (open-price :accessor open-price-of :initform 0.0)
   (high-price :accessor high-price-of :initform 0.0)
   (low-price :accessor low-price-of :initform 0.0)
   (close-price :accessor close-price-of :initform 0.0)))
