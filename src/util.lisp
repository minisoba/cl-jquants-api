(in-package :cl-jquants-api)

(defun %decode-json-key (text)
  (substitute #\- #\_ (cl-change-case:snake-case text)))

(defmacro make-jquants-instance-from-hash-table (&key class hash-table alternative-keys key-map overflow-slot)
  `(let ((instance (make-instance ',class)))
     ,@(when overflow-slot
         `((setf (slot-value instance ',overflow-slot) (make-hash-table :test 'equal))))
     (maphash
      (lambda (key value)
        (let* ((km-entry (when ,key-map
                           (assoc key ,key-map :test #'string=)))
               (mapped-key
                 (if km-entry
                     (cdr km-entry)
                     (let* ((converted-key
                              (intern (string-upcase (%decode-json-key key)) :cl-jquants-api)))
                       (or (cdr (assoc converted-key ,alternative-keys))
                           converted-key)))))
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
              ,(if overflow-slot
                   `(setf (gethash key (slot-value instance ',overflow-slot)) value)
                   `(warn "Invalid key ~a for class ~a" key ',class)))))
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

(defmacro %make-jquants-instance (&key class node endpoint params alternative-keys key-map)
  `(let ((base-url (%build-url ,endpoint ,params))
         (instances '()))
     (loop
       with pagination-key
       for url = base-url
         then (format nil "~a~apagination_key=~a"
                      base-url
                      (if (find #\? base-url) "&" "?")
                      pagination-key)
       do (let ((response (perform-http-request url :method :get)))
            (unless response
              (warn "Empty response from API endpoint: ~a" url)
              (return))
            (setf pagination-key (gethash "pagination_key" response))
            (dolist (hash-table (gethash ,node response))
              (push (make-jquants-instance-from-hash-table
                     :class ,class
                     :hash-table hash-table
                     :alternative-keys ,alternative-keys
                     :key-map ,key-map)
                    instances)))
       while pagination-key)
     (nreverse instances)))

(defmacro create-jquants-instance (&key class node endpoint params alternative-keys key-map)
  `(%make-jquants-instance
    :class ,class
    :node ,node
    :endpoint ,endpoint
    :params ,params
    :alternative-keys ,alternative-keys
    :key-map ,key-map))
