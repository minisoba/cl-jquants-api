(in-package :cl-jquants-api)

(defun %decode-json-key (text)
  (substitute #\- #\_ (cl-change-case:snake-case text)))

(defmacro make-jquants-instance-from-hash-table (&key class hash-table alternative-keys)
  `(let ((instance (make-instance ',class)))
     (maphash
      (lambda (key value)
        (let* ((converted-key
                (intern (string-upcase (%decode-json-key key)) :cl-jquants-api))
               (mapped-key
                (or (cdr (assoc converted-key ,alternative-keys))
                    converted-key)))
          (if (cl-jquants-api::slot-exists-p instance mapped-key)
              (let ((mapped-value (slot-value instance mapped-key)))
                (cond ((and (stringp value) (zerop (length value)))
                       (setf (slot-value instance mapped-key) nil))
                      ((numberp mapped-value)
                       (setf (slot-value instance mapped-key)
                             (handler-case
                                 (parse-number:parse-number value)
                               (error () value))))
                      (t
                       (setf (slot-value instance mapped-key) value)))
                (cl-jquants-api::handle-slot-value instance mapped-key value))
            (warn "Invalid key ~a for class ~a" key ',class))))
      ,hash-table)
     (cl-jquants-api::complete-object-update instance)
     instance))

(defun %build-url (endpoint params)
  (let ((args (mapcar (lambda (pair)
                        ;; only downcase a parameter name but not value
                        (format nil "~a=~a" (string-downcase (car pair)) (cdr pair)))
                      (remove-if-not #'cdr params))))
    (if (null args)
        endpoint
        (format nil "~a?~{~a~^&~}" endpoint args))))

(defmacro %make-jquants-instance (&key class node endpoint params alternative-keys)
  `(let ((url (%build-url ,endpoint ,params))
         (instances '()))
     (loop
       with pagination-key
       do (let ((response (perform-http-request url :method :get)))
            (setf pagination-key (gethash "pagination_key" response))
            (dolist (hash-table (gethash ,node response))
              (push (make-jquants-instance-from-hash-table
                     :class ,class
                     :hash-table hash-table
                     :alternative-keys ,alternative-keys)
                    instances)))
       while pagination-key
       do (setf url (format nil "~a&pagination_key=~a" url pagination-key)))
     (nreverse instances)))

(defmacro create-jquants-instance (&key class node endpoint params alternative-keys)
  `(%make-jquants-instance
    :class ,class
    :node ,node
    :endpoint ,endpoint
    :params ,params
    :alternative-keys ,alternative-keys))
