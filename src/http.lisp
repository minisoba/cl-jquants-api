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

(defmethod send-request ((obj http-handler) endpoint method
                         &key (content nil) (stream *standard-output*) (timeout 20))
  (let* ((url (format nil "~a/~a" +end-point-base+ endpoint))
         (headers (%build-headers)))
    ;; JPX's responses contain UTF-8 (Japanese names, EU addresses with
    ;; umlauts, etc.) but the HTTP response Content-Type doesn't declare a
    ;; charset, so drakma silently falls back to Latin-1. Request raw
    ;; octets via :force-binary and decode UTF-8 ourselves before handing
    ;; the body to yason — the previous setf-external-format trick was a
    ;; no-op against drakma's already-bound stream and produced mojibake
    ;; like "Z?rich".
    (multiple-value-bind (octets status-code _headers _uri _stream _must-close reason-phrase)
        (if (eq method :get)
            (drakma:http-request
             url :method :get :additional-headers headers
                 :connection-timeout timeout :force-binary t)
            (drakma:http-request
             url :method :post :additional-headers headers
                 :content-type "application/json"
                 :connection-timeout timeout :content content :force-binary t))
      (declare (ignore _headers _uri _stream _must-close))
      (format stream "Sending HTTP request: ~a~%" url)
      (let ((body (flexi-streams:octets-to-string octets :external-format :utf-8)))
        (when-let (hash-table (yason:parse body))
          (cond ((= status-code 200)
                 hash-table)
                (t
                 (error 'http-request-error
                        :status status-code
                        :reason reason-phrase
                        :text   (gethash "message" hash-table)))))))))

(defun perform-http-request (url &key (method :get) content (stream nil))
  (send-request cl-jquants-api::*http-handler* url method :content content :stream (or stream *standard-output*)))
