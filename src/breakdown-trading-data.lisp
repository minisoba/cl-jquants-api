(in-package :cl-jquants-api)

(defclass breakdown-trading-data (jquants-object)
  ((code :accessor code-of :initform nil)
   (date :accessor date-of :initform 0)
   (long-sell-value :accessor long-sell-value-of :initform 0.0)
   (short-sell-without-margin-value :accessor short-sell-without-margin-value-of :initform 0.0)
   (margin-sell-new-value :accessor margin-sell-new-value-of :initform 0.0)
   (margin-sell-close-value :accessor margin-sell-close-value-of :initform 0.0)
   (long-buy-value :accessor long-buy-value-of :initform 0.0)
   (margin-buy-new-value :accessor margin-buy-new-value-of :initform 0.0)
   (margin-buy-close-value :accessor margin-buy-close-value-of :initform 0.0)
   (long-sell-volume :accessor long-sell-volume-of :initform 0.0)
   (short-sell-without-margin-volume :accessor short-sell-without-margin-volume-of :initform 0.0)
   (margin-sell-new-volume :accessor margin-sell-new-volume-of :initform 0.0)
   (margin-sell-close-volume :accessor margin-sell-close-volume-of :initform 0.0)
   (long-buy-volume :accessor long-buy-volume-of :initform 0.0)
   (margin-buy-new-volume :accessor margin-buy-new-volume-of :initform 0.0)
   (margin-buy-close-volume :accessor margin-buy-close-volume-of :initform 0.0)))
