(in-package :cl-jquants-api.test)

(def-suite* parquet-test :in cl-jquants-api-tests)

;;; Helper: create a minimal jquants-like test object
(defclass test-stock-prices (cl-jquants-api:stock-prices)
  ((code :initarg :code :initform nil :accessor test-code)
   (date :initarg :date :initform 0 :accessor test-date)
   (open-price :initarg :open-price :initform 0.0 :accessor test-open-price)
   (close-price :initarg :close-price :initform 0.0 :accessor test-close-price)
   (volume :initarg :volume :initform 0.0 :accessor test-volume)))

(defun make-test-stock-prices (&key code date open-price close-price volume)
  (make-instance 'test-stock-prices
                 :code code
                 :date date
                 :open-price open-price
                 :close-price close-price
                 :volume volume))

;;; Tests for sql-escape

(test sql-escape-nil
  "NULL values should produce SQL NULL"
  (is (string= "NULL" (cl-jquants-api::%sql-escape nil))))

(test sql-escape-undetermined
  ":undetermined should produce SQL NULL"
  (is (string= "NULL" (cl-jquants-api::%sql-escape :undetermined))))

(test sql-escape-integer
  "Integers should format without suffix"
  (is (string= "42" (cl-jquants-api::%sql-escape 42))))

(test sql-escape-double-float
  "Double-floats should format without d0 suffix"
  (let ((result (cl-jquants-api::%sql-escape 4070.0d0)))
    (is (not (search "d0" result)))
    (is (not (search "f0" result)))))

(test sql-escape-single-float
  "Single-floats should format without f0 suffix"
  (let ((result (cl-jquants-api::%sql-escape 0.0f0)))
    (is (not (search "f0" result)))
    (is (not (search "d0" result)))))

(test sql-escape-string
  "Strings should be single-quoted"
  (is (string= "'hello'" (cl-jquants-api::%sql-escape "hello"))))

(test sql-escape-string-with-quotes
  "Single quotes in strings should be escaped"
  (is (string= "'it''s'" (cl-jquants-api::%sql-escape "it's"))))

(test sql-escape-integer-timestamp
  "Unix epoch integer with TIMESTAMP sql-type should wrap with to_timestamp"
  (is (string= "to_timestamp(1776038400)"
               (cl-jquants-api::%sql-escape 1776038400 "TIMESTAMP")))
  ;; Without sql-type, integer should format as plain number
  (is (string= "1776038400"
               (cl-jquants-api::%sql-escape 1776038400))))

(test convert-string-to-epoch-time-non-string
  "Non-string input (e.g. timestamp initform) should return NIL, not crash"
  (is (null (cl-jquants-api::%convert-string-to-epoch-time nil)))
  (is (null (cl-jquants-api::%convert-string-to-epoch-time 0)))
  (is (null (cl-jquants-api::%convert-string-to-epoch-time
             (local-time:universal-to-timestamp 0))))
  ;; Valid string input should still return an epoch integer
  (is (integerp (cl-jquants-api::%convert-string-to-epoch-time "2026-04-13"))))

;;; Tests for make-create-table-sql

(test make-create-table-sql-generates-valid-ddl
  "Should generate a CREATE TABLE statement with correct columns"
  (let* ((obj (make-test-stock-prices :code "13010" :date 1735257600
                                      :open-price 4070.0d0
                                      :close-price 4115.0d0
                                      :volume 29500.0d0))
         (sql (cl-jquants-api::%make-create-table-sql "test_table" obj)))
    (is (search "CREATE TABLE test_table" sql))
    (is (search "code" sql))
    (is (search "date" sql))
    (is (search "open_price" sql))
    (is (search "close_price" sql))
    (is (search "volume" sql))))

;;; Tests for make-insert-sql

(test make-insert-sql-generates-valid-insert
  "Should generate an INSERT statement with correct values"
  (let* ((obj (make-test-stock-prices :code "13010" :date 1735257600
                                      :open-price 4070.0d0
                                      :close-price 4115.0d0
                                      :volume 29500.0d0))
         (sql (cl-jquants-api::%make-insert-sql "test_table" obj)))
    (is (search "INSERT INTO test_table VALUES" sql))
    (is (search "'13010'" sql))
    (is (search "1735257600" sql))
    (is (not (search "d0" sql)))
    (is (not (search "f0" sql)))))

;;; Tests for save-to-parquet (integration)

(test save-to-parquet-creates-file
  "Should create a Parquet file on disk"
  (uiop:with-temporary-file (:pathname parquet-path
                             :type "parquet"
                             :keep t)
    (let ((objects (list
                    (make-test-stock-prices :code "13010" :date 1735257600
                                            :open-price 4070.0d0
                                            :close-price 4115.0d0
                                            :volume 29500.0d0)
                    (make-test-stock-prices :code "13050" :date 1735257600
                                            :open-price 2000.0d0
                                            :close-price 2050.0d0
                                            :volume 10000.0d0))))
      (unwind-protect
           (progn
             (cl-jquants-api::save-to-parquet objects parquet-path)
             (is (probe-file parquet-path)))
        (when (probe-file parquet-path)
          (delete-file parquet-path))))))

(test save-to-parquet-empty-list
  "Should return nil and not create a file for empty list"
  (let ((parquet-path "/tmp/test-empty-parquet.parquet"))
    (is (null (cl-jquants-api::save-to-parquet nil parquet-path)))
    (is (not (probe-file parquet-path)))))

(test save-to-parquet-verifies-content-via-duckdb
  "Should produce a Parquet file readable by DuckDB with correct row count"
  (let* ((parquet-path (format nil "/tmp/test-parquet-verify-~a.parquet"
                               (get-universal-time)))
         (objects (list
                   (make-test-stock-prices :code "13010" :date 1735257600
                                           :open-price 4070.0d0
                                           :close-price 4115.0d0
                                           :volume 29500.0d0)
                   (make-test-stock-prices :code "13050" :date 1735344000
                                           :open-price 2000.0d0
                                           :close-price 2050.0d0
                                           :volume 10000.0d0)
                   (make-test-stock-prices :code "13060" :date 1735430400
                                           :open-price 500.0d0
                                           :close-price 510.0d0
                                           :volume 5000.0d0))))
    (unwind-protect
         (progn
           (cl-jquants-api::save-to-parquet objects parquet-path)
           (duckdb:with-open-database (db)
             (duckdb:with-open-connection (conn db)
               (let ((result (duckdb:query
                              (format nil "SELECT count(*) FROM '~a'" parquet-path)
                              nil :connection conn)))
                 ;; Result: (("count_star()" . #(3)))
                 (is (= 3 (aref (cdar result) 0)))))))
      (when (probe-file parquet-path)
        (delete-file parquet-path)))))

