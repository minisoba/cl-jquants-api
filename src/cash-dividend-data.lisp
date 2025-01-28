(in-package :cl-jquants-api)

(defclass cash-dividend-data (jquants-object)
  ((announcement-date :accessor announcement-date-of :initform 0)
   (announcement-time :accessor announcement-time-of :initform 0)
   (code :accessor code-of :initform nil)
   (reference-number :accessor reference-number-of :initform nil)
   (status-code :accessor status-code-of :initform nil)
   (board-meeting-date :accessor board-meeting-date-of :initform 0)
   (interim-final-code :accessor interim-final-code-of :initform nil)
   (forecast-result-code :accessor forecast-result-code-of :initform nil)
   (interim-final-term :accessor interim-final-term-of :initform nil)
   (gross-dividend-rate :accessor gross-dividend-rate-of :initform 0.0)
   (record-date :accessor record-date-of :initform 0)
   (ex-date :accessor ex-date-of :initform 0)
   (actual-record-date :accessor actual-record-date-of :initform 0)
   (payable-date :accessor payable-date-of :initform 0)
   (ca-reference-number :accessor ca-reference-number-of :initform 0.0)
   (distribution-amount :accessor distribution-amount-of :initform 0.0)
   (retained-earnings :accessor retained-earnings-of :initform 0.0)
   (deemed-dividend :accessor deemed-dividend-of :initform 0.0)
   (deemed-capital-gains :accessor deemed-capital-gains-of :initform 0.0)
   (net-asset-decrease-ratio :accessor net-asset-decrease-ratio-of :initform 0.0)
   (commemorative-special-code :accessor commemorative-special-code-of :initform nil)
   (commemorative-dividend-rate :accessor commemorative-dividend-rate-of :initform 0.0)
   (special-dividend-rate :accessor special-dividend-rate-of :initform 0.0)))

(defmethod complete-object-update ((obj cash-dividend-data))
  (flet ((parse-and-set-number (accessor)
           (handler-case
               (setf (slot-value obj accessor)
                     (parse-number:parse-number (slot-value obj accessor)))
             (error ()
               (setf (slot-value obj accessor) nil))))
         (parse-and-set-undetermined (accessor)
           (let ((value (slot-value obj accessor)))
             (setf (slot-value obj accessor)
                   (cond ((string= value "-")
                          :undetermined)
                         ((null value)
                          :not-applicable)
                         (t
                          value))))))
    (mapcar #'parse-and-set-number
            (list 'status-code
                  'interim-final-code
                  'forecast-result-code
                  'commemorative-special-code))
    (mapcar #'parse-and-set-undetermined
            (list 'gross-dividend-rate
                  'payable-date
                  'commemorative-dividend-rate))))
