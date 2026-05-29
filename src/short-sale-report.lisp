(in-package :cl-jquants-api)

(defclass short-sale-report (jquants-object)
  ((code :accessor code-of :initform nil)
   (disc-date :accessor disc-date-of :initform (local-time:universal-to-timestamp 0))
   (calc-date :accessor calc-date-of :initform (local-time:universal-to-timestamp 0))
   (short-seller-name :accessor short-seller-name-of :initform nil)
   (short-seller-address :accessor short-seller-address-of :initform nil)
   (discretionary-investment-contractor-name
    :accessor discretionary-investment-contractor-name-of :initform nil)
   (discretionary-investment-contractor-address
    :accessor discretionary-investment-contractor-address-of :initform nil)
   (fund-name :accessor fund-name-of :initform nil)
   (short-positions-to-shares-outstanding-ratio
    :accessor short-positions-to-shares-outstanding-ratio-of :initform 0.0)
   (short-positions-in-shares-number
    :accessor short-positions-in-shares-number-of :initform 0.0)
   (short-positions-in-units-number
    :accessor short-positions-in-units-number-of :initform 0.0)
   (short-positions-in-previous-reporting-ratio
    :accessor short-positions-in-previous-reporting-ratio-of :initform 0.0)
   (short-positions-in-previous-reporting-date
    :accessor short-positions-in-previous-reporting-date-of
    :initform (local-time:universal-to-timestamp 0))
   (notes :accessor notes-of :initform nil)))
