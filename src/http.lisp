(in-package :cl-jquants-api)

(defvar *api-key* (uiop:getenv "JQUANTS_API_KEY"))

(define-condition http-request-error (jquants-error)
  ((status :reader status :initarg :status)
   (reason :reader reason :initarg :reason)
   (text   :reader text   :initarg :text))
  (:report (lambda (condition stream)
             (format stream "HTTP Error ~a ~a: ~a" 
                     (status condition) 
                     (reason condition) 
                     (text   condition)))))

(defclass http-handler ()
  ())

(defvar *http-handler* (make-instance 'http-handler))

(defun %build-headers ()
  `(("x-api-key" . ,*api-key*)))

;;; ---------------------------------------------------------------------------
;;; Client-side rate limiting (gap #7)
;;;
;;; amx-scheduler fires download tasks concurrently (daily chain /
;;; backfill), so the only enforceable place to be a good J-Quants
;;; citizen is right here at the single HTTP chokepoint. Two parts:
;;;
;;;  1. a global min-interval gate (token-bucket-ish): at most one
;;;     request every *MIN-REQUEST-INTERVAL* seconds, serialized by a
;;;     lock so concurrent callers pace correctly (not just
;;;     approximately).
;;;  2. honour HTTP 429 / 503 `Retry-After`: sleep and retry up to
;;;     *MAX-RETRY-AFTER-ATTEMPTS* (the authoritative backstop - the
;;;     server tells us exactly how long to wait).
;;;
;;; Defaults are conservative and env-tunable so deployment can tighten
;;; or relax without a code change. Interval 0 disables the gate.
;;; ---------------------------------------------------------------------------

(defun %env-number (name default)
  (let ((v (uiop:getenv name)))
    (or (and v (ignore-errors (let ((n (read-from-string v))) (and (numberp n) n))))
        default)))

(defvar *min-request-interval*
  (%env-number "JQUANTS_MIN_REQUEST_INTERVAL" 0.2)
  "Minimum seconds between outbound J-Quants HTTP requests (global,
across threads). 0 disables. Env: JQUANTS_MIN_REQUEST_INTERVAL.")

(defvar *max-retry-after-attempts*
  (truncate (%env-number "JQUANTS_MAX_RETRY_AFTER_ATTEMPTS" 5))
  "Max retries on HTTP 429/503 honouring Retry-After. Env:
JQUANTS_MAX_RETRY_AFTER_ATTEMPTS.")

(defvar *rate-lock* (bt:make-lock "jquants-rate-limit"))
(defvar *next-allowed-at* 0
  "internal-real-time units before which the next request must wait.")

(defun %throttle ()
  "Block until the global min-interval since the previous request has
elapsed. Holding the lock across the wait serializes pacing so N
concurrent callers are spaced, not bursted."
  (when (and *min-request-interval* (plusp *min-request-interval*))
    (bt:with-lock-held (*rate-lock*)
      (let* ((now (get-internal-real-time))
             (wait (- *next-allowed-at* now)))
        (when (plusp wait)
          (sleep (/ wait internal-time-units-per-second)))
        (setf *next-allowed-at*
              (+ (max now *next-allowed-at*)
                 (round (* *min-request-interval*
                           internal-time-units-per-second))))))))

(defun %retry-after-seconds (headers)
  "Parse a Retry-After header (delay-seconds form) from HEADERS, or
NIL. The HTTP-date form is rare for J-Quants; fall back to caller
backoff when absent/unparseable."
  (let ((v (cdr (assoc :retry-after headers))))
    (and v (ignore-errors
             (let ((n (parse-integer (string-trim " " v) :junk-allowed t)))
               (and n (plusp n) n))))))

(defmethod send-request ((obj http-handler) endpoint method
                         &key (content nil) (stream *standard-output*) (timeout 20))
  (let* ((url (format nil "~a/~a" +end-point-base+ endpoint))
         (headers (%build-headers))
         (attempt 0))
    (loop
      (%throttle)
      (multiple-value-bind (body-or-stream status-code resp-headers
                            _uri _stream _must-close reason-phrase)
          (if (eq method :get)
              (drakma:http-request
               url :method :get :additional-headers headers
                   :connection-timeout timeout :want-stream t)
              (drakma:http-request
               url :method :post :additional-headers headers
                   :content-type "application/json"
                   :connection-timeout timeout :content content :want-stream t))
        (declare (ignore _uri _stream _must-close))
        (format stream "Sending HTTP request: ~a~%" url)
        (cond
          ;; Retryable: server-directed backoff (429) or transient
          ;; unavailability (503). Honour Retry-After; else linear.
          ((and (member status-code '(429 503))
                (< attempt *max-retry-after-attempts*))
           (incf attempt)
           (let ((wait (or (%retry-after-seconds resp-headers) (* 2 attempt))))
             (ignore-errors (close body-or-stream))
             (format stream
                     "jquants: HTTP ~a; retry ~a/~a after ~as~%"
                     status-code attempt *max-retry-after-attempts* wait)
             (sleep wait)))
          (t
           ;; JPX responses are UTF-8 (Japanese names, EU addresses with
           ;; umlauts) but Content-Type doesn't declare a charset, so
           ;; drakma silently falls back to Latin-1 producing mojibake
           ;; ("Z?rich" instead of "Zürich"). Force the stream's
           ;; external-format to UTF-8 before yason consumes it.
           (setf (flexi-streams:flexi-stream-external-format body-or-stream)
                 :utf-8)
           (when-let (hash-table (yason:parse body-or-stream))
             (return
               (cond ((= status-code 200) hash-table)
                     (t (error 'http-request-error
                               :status status-code
                               :reason reason-phrase
                               :text (gethash "message" hash-table))))))
           ;; empty body on a non-200 (or 200 with no JSON): nothing
           ;; to return -> NIL, same as the original behaviour.
           (return nil)))))))

(defun perform-http-request (url &key (method :get) content (stream nil))
  (send-request cl-jquants-api::*http-handler* url method :content content :stream (or stream *standard-output*)))
