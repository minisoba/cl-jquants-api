(in-package :cl-jquants-api)

(defun %convert-string-to-epoch-time (old-value)
  (when-let (new-value (local-time:parse-timestring
                        old-value
                        :fail-on-error nil :allow-missing-date-part t))
    (local-time:timestamp-to-unix new-value)))

(defun %convert-string-to-seconds-of-day (old-value)
  (let ((new-value (local-time:parse-timestring
                    old-value
                    :fail-on-error nil :allow-missing-date-part t)))
    (cond (new-value
           (local-time:sec-of new-value))
          (t
           (when (cl-ppcre:scan "[0-9]{2}:[0-9]{2}" old-value)
             (local-time:sec-of
              (local-time:parse-timestring (format nil "~a:00" old-value))))))))

(defclass jquants-object ()
  ()
  (:documentation ""))

(defmethod print-object ((obj jquants-object) stream)
  (let ((class (class-of obj)))
    (format stream "#<~a: ~{~a~^ ~}>"
            (class-name class)
            (loop for slot in (closer-mop:class-direct-slots class)
                  collect (format nil "~a=~a"
                                  (closer-mop:slot-definition-name slot)
                                  (slot-value obj (closer-mop:slot-definition-name slot)))))))

(defmethod handle-slot-value ((obj jquants-object) slot-name new-value)
  (when (or (eq slot-name 'date)
            (eq slot-name 'start-date)
            (eq slot-name 'end-date)
            (eq slot-name 'ex-date)
            (eq slot-name 'last-trading-day)
            (eq slot-name 'special-quotation-day)
            (eq slot-name 'announcement-date)
            (eq slot-name 'record-date)
            (eq slot-name 'payable-date)
            (eq slot-name 'published-date)
            (eq slot-name 'disclosed-date)
            (eq slot-name 'board-meeting-date)
            (eq slot-name 'actual-record-date)
            (eq slot-name 'current-period-start-date)
            (eq slot-name 'current-period-end-date)
            (eq slot-name 'current-fiscal-year-start-date)
            (eq slot-name 'current-fiscal-year-end-date)
            (eq slot-name 'next-fiscal-year-start-date)
            (eq slot-name 'next-fiscal-year-end-date))
    (cond ((string= new-value "-")
           (setf (slot-value obj slot-name) :undetermined))
          ((string= new-value "")
           (setf (slot-value obj slot-name) nil))
          (t
           (setf (slot-value obj slot-name) (%convert-string-to-epoch-time new-value)))))
  (when (or (eq slot-name 'disclosed-time)
            (eq slot-name 'announcement-time))
    (setf (slot-value obj slot-name) (%convert-string-to-seconds-of-day new-value))))

(defmethod complete-object-update ((obj jquants-object))
  )
