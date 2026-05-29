(in-package :cl-jquants-api)

;; PubReason is a nested object on each margin-alert row — flags for the six
;; classifications that can trigger Daily Publication. Modelled as its own
;; class the same way financial-statement-data wraps its FS sub-object.
(defclass margin-alert-publication-reason (jquants-object)
  ((restricted                     :accessor restricted-of                     :initform nil)
   (daily-publication              :accessor daily-publication-of              :initform nil)
   (monitoring                     :accessor monitoring-of                     :initform nil)
   (restricted-by-jsf              :accessor restricted-by-jsf-of              :initform nil)
   (precaution-by-jsf              :accessor precaution-by-jsf-of              :initform nil)
   (unclear-or-securities-on-alert :accessor unclear-or-securities-on-alert-of :initform nil)))

(defclass margin-alert (jquants-object)
  ((code                                   :accessor code-of                                   :initform nil)
   (published-date                         :accessor published-date-of                         :initform (local-time:universal-to-timestamp 0))
   (application-date                       :accessor application-date-of                       :initform (local-time:universal-to-timestamp 0))
   ;; Nested PubReason sub-object — populated by handle-slot-value below.
   (publication-reason                     :accessor publication-reason-of                     :initform nil)
   (publication-reason-raw                 :accessor publication-reason-raw-of                 :initform nil)
   ;; Outstanding positions + their prev-day change + ratio. Change/Ratio
   ;; fields are number-or-string in the wire ('-' if no prev-day data,
   ;; '*' for ETFs that don't track listed-shares ratio).
   (short-margin-outstanding               :accessor short-margin-outstanding-of               :initform 0.0)
   (short-margin-outstanding-change        :accessor short-margin-outstanding-change-of        :initform nil)
   (short-margin-outstanding-ratio         :accessor short-margin-outstanding-ratio-of         :initform nil)
   (long-margin-outstanding                :accessor long-margin-outstanding-of                :initform 0.0)
   (long-margin-outstanding-change         :accessor long-margin-outstanding-change-of         :initform nil)
   (long-margin-outstanding-ratio          :accessor long-margin-outstanding-ratio-of          :initform nil)
   (short-long-ratio                       :accessor short-long-ratio-of                       :initform 0.0)
   (short-negotiable-margin-outstanding    :accessor short-negotiable-margin-outstanding-of    :initform 0.0)
   (short-negotiable-margin-outstanding-change
    :accessor short-negotiable-margin-outstanding-change-of    :initform nil)
   (short-standardized-margin-outstanding  :accessor short-standardized-margin-outstanding-of  :initform 0.0)
   (short-standardized-margin-outstanding-change
    :accessor short-standardized-margin-outstanding-change-of  :initform nil)
   (long-negotiable-margin-outstanding     :accessor long-negotiable-margin-outstanding-of     :initform 0.0)
   (long-negotiable-margin-outstanding-change
    :accessor long-negotiable-margin-outstanding-change-of     :initform nil)
   (long-standardized-margin-outstanding   :accessor long-standardized-margin-outstanding-of   :initform 0.0)
   (long-standardized-margin-outstanding-change
    :accessor long-standardized-margin-outstanding-change-of   :initform nil)
   (tse-margin-regulation-classification   :accessor tse-margin-regulation-classification-of   :initform nil)))

(defmethod handle-slot-value :after ((obj margin-alert) slot-name new-value)
  ;; When the publication-reason slot lands as a hash-table, instantiate the
  ;; sub-class so callers get typed accessors on the nested object instead
  ;; of having to gethash into the raw map.
  (when (and (eq slot-name 'publication-reason)
             (hash-table-p new-value))
    (setf (publication-reason-raw-of obj) new-value)
    (setf (publication-reason-of obj)
          (make-jquants-instance-from-hash-table
           :class margin-alert-publication-reason
           :hash-table new-value
           :key-map '(("Restricted"          . restricted)
                      ("DailyPublication"    . daily-publication)
                      ("Monitoring"          . monitoring)
                      ("RestrictedByJSF"     . restricted-by-jsf)
                      ("PrecautionByJSF"     . precaution-by-jsf)
                      ("UnclearOrSecOnAlert" . unclear-or-securities-on-alert))))))
