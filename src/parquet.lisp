(in-package :cl-jquants-api)

(defun %slot-definitions (obj)
  (loop for slot in (closer-mop:class-direct-slots (class-of obj))
        for name = (closer-mop:slot-definition-name slot)
        for initform = (closer-mop:slot-definition-initform slot)
        collect (cons name
                      (cond ((eql initform 0)   "BIGINT")
                            ((eql initform 0.0) "DOUBLE")
                            (t                  "VARCHAR")))))

(defun %slot-values (obj)
  (loop for slot in (closer-mop:class-direct-slots (class-of obj))
        for name = (closer-mop:slot-definition-name slot)
        collect (cons name (slot-value obj name))))

(defun %make-create-table-sql (table-name obj)
  (format nil "CREATE TABLE ~a (~{~a~^, ~})"
          table-name
          (loop for (name . sql-type) in (%slot-definitions obj)
                collect (format nil "~a ~a"
                                (substitute #\_ #\- (string-downcase (symbol-name name)))
                                sql-type))))

(defun %sql-escape (value)
  (cond ((floatp value)  (format nil "~f" value))
        ((numberp value) (format nil "~a" value))
        ((null value)    "NULL")
        ((eq value :undetermined) "NULL")
        (t (format nil "'~a'" (cl-ppcre:regex-replace-all "'" (princ-to-string value) "''")))))

(defun %make-insert-sql (table-name obj)
  (let ((cols (%slot-definitions obj))
        (vals (%slot-values obj)))
    (format nil "INSERT INTO ~a VALUES (~{~a~^, ~})"
            table-name
            (loop for (name . _) in cols
                  for val = (cdr (assoc name vals))
                  collect (%sql-escape val)))))

(defun save-to-parquet (objects parquet-path &key (table-name "jquants_data"))
  "Write a list of jquants-objects to a Parquet file via DuckDB."
  (when (null objects) (return-from save-to-parquet nil))
  (let* ((first-obj (first objects))
         (expected-slot-defs (%slot-definitions first-obj))
         (expected-type (type-of first-obj)))
    (duckdb:with-open-database (db)
      (duckdb:with-open-connection (conn db)
        (duckdb:query (%make-create-table-sql table-name first-obj) nil :connection conn)
        (duckdb:query "BEGIN" nil :connection conn)
        (dolist (obj objects)
          (unless (and (eq (type-of obj) expected-type)
                       (equal expected-slot-defs (%slot-definitions obj)))
            (error "SAVE-TO-PARQUET expects all objects to have the same type and slot layout. Expected type ~S with slots ~S, but got type ~S with slots ~S."
                   expected-type
                   (mapcar #'car expected-slot-defs)
                   (type-of obj)
                   (mapcar #'car (%slot-definitions obj))))
          (duckdb:query (%make-insert-sql table-name obj) nil :connection conn))
        (duckdb:query "COMMIT" nil :connection conn)
        (duckdb:query
          (format nil "COPY ~a TO ~a (FORMAT PARQUET)"
                  table-name
                  (%sql-escape parquet-path))
          nil :connection conn)))))
