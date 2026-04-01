(in-package :cl-jquants-api)

(defvar *api-token* nil)
(defvar *refresh-token* nil)

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
  `(("Authorization" . ,(format nil "Bearer ~a" *api-token*))))

(defmethod send-request ((obj http-handler) endpoint method
                         &key (content nil) (stream *standard-output*) (timeout 20))
  (let* ((url (format nil "~a/~a" +end-point-base+ endpoint))
         (headers (%build-headers)))
    (multiple-value-bind (body-or-stream status-code _headers _uri _stream _must-close reason-phrase)
        (if (eq method :get)
            (drakma:http-request
             url :method :get :additional-headers headers :connection-timeout timeout :want-stream t)
            (drakma:http-request
             url :method :post :additional-headers headers :content-type "application/json"
                 :connection-timeout timeout :content content :want-stream t))
      (declare (ignore _headers _uri _stream _must-close))
      (format stream "Sending HTTP request: ~a~%" url)
      (setf (flexi-streams:flexi-stream-external-format body-or-stream) :utf-8)
      (when-let (hash-table (yason:parse body-or-stream))
        (cond ((= status-code 200)
               hash-table)
              (t
               (error 'http-request-error 
                      :status status-code
                      :reason reason-phrase
                      :text   (gethash "message" hash-table))))))))

(defun perform-http-request (url &key (method :get) content (stream nil))
  (handler-case
      (send-request cl-jquants-api::*http-handler* url method :content content :stream (or stream *standard-output*))
    (http-request-error (err)
      (when stream
        (format stream "Caught HTTP error: ~a~%" err))
      nil)
    (error (err)
      (when stream
        (format stream "Unexpected error during HTTP request: ~a~%" err))
      nil)))
