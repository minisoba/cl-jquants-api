;;; scripts/download.lisp
;;;
;;; Standalone batch-download script for the cl-jquants-api library.
;;;
;;; Usage (run with SBCL from the repository root):
;;;
;;;   sbcl --load scripts/download.lisp -- [DATE] [OUTPUT-DIR]
;;;
;;; Arguments:
;;;   DATE        - ISO-8601 date string (YYYY-MM-DD).  Defaults to yesterday.
;;;   OUTPUT-DIR  - Directory to write Parquet files into.
;;;                 Defaults to "output/<DATE>/".
;;;
;;; Environment variables:
;;;   JQUANTS_API_KEY - J-Quants API key (required).
;;;
;;; Each dataset is saved to <OUTPUT-DIR>/<dataset-name>.parquet.
;;; The script exits with status 0 on success or 1 if any dataset fails.

(in-package :cl-user)

;;;; ---------- bootstrap -------------------------------------------------------

;; Load Quicklisp when running as a plain --load script.
(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql-setup)
    (load ql-setup)))

;; Register the repository root so ASDF can find cl-jquants-api.
(let ((repo-root
        (or (ignore-errors
              (truename
               (merge-pathnames "../"
                                (load-time-value *load-truename*))))
            *default-pathname-defaults*)))
  (pushnew repo-root asdf:*central-registry* :test #'equal))

(ql:quickload :cl-jquants-api :silent t)

;;;; ---------- package ---------------------------------------------------------

(defpackage :jquants-download
  (:use :cl :cl-jquants-api)
  (:export #:main #:download-dataset #:datasets))

(in-package :jquants-download)

;;;; ---------- helpers ---------------------------------------------------------

(defun format-date (timestamp)
  "Format a local-time timestamp as YYYY-MM-DD."
  (local-time:format-timestring
   nil timestamp
   :format '(:year "-" (:month 2) "-" (:day 2))))

(defun yesterday ()
  "Return yesterday's date as a YYYY-MM-DD string."
  (format-date (local-time:timestamp- (local-time:now) 1 :day)))

(defun ensure-directory (path)
  "Create PATH and all parent directories if they do not already exist."
  (uiop:ensure-all-directories-exist
   (list (uiop:ensure-directory-pathname path))))

(defun dataset-path (output-dir name)
  "Return the full Parquet file path for a dataset named NAME inside OUTPUT-DIR."
  (merge-pathnames (make-pathname :name name :type "parquet")
                   (uiop:ensure-directory-pathname output-dir)))

;;;; ---------- dataset definitions --------------------------------------------

(defstruct dataset
  name          ; string  – base name used for the output file
  fetcher)      ; (lambda (date) ...) – returns a list of jquants-objects

(defun %datasets (date)
  "Return the list of datasets to download for DATE (a YYYY-MM-DD string)."
  (list
   (make-dataset
    :name    "listed-issues"
    :fetcher (lambda (d) (get-listed-issue-master :date d)))
   (make-dataset
    :name    "stock-prices"
    :fetcher (lambda (d) (get-stock-prices :date d)))
   (make-dataset
    :name    "morning-session-stock-prices"
    :fetcher (lambda (_) (declare (ignore _)) (get-morning-session-stock-prices)))
   (make-dataset
    :name    "indices-prices"
    :fetcher (lambda (d) (get-indices-prices :date d)))
   (make-dataset
    :name    "topix-prices"
    :fetcher (lambda (d) (get-topix-prices :from d :to d)))
   (make-dataset
    :name    "index-option-prices"
    :fetcher (lambda (d) (get-index-option-prices :date d)))
   (make-dataset
    :name    "trading-by-type-of-investors"
    :fetcher (lambda (d) (get-trading-by-type-of-investors :from d :to d)))
   (make-dataset
    :name    "margin-trading-outstandings"
    :fetcher (lambda (d) (get-margin-trading-outstandings :date d)))
   (make-dataset
    :name    "short-sale-value-and-ratio-by-sector"
    :fetcher (lambda (d) (get-short-sale-value-and-ratio-by-sector :date d)))
   (make-dataset
    :name    "trading-calendar"
    :fetcher (lambda (d) (get-trading-calendar :from d :to d)))
   (make-dataset
    :name    "earnings-calendar"
    :fetcher (lambda (_) (declare (ignore _)) (get-earnings-calendar)))
   (make-dataset
    :name    "financial-data"
    :fetcher (lambda (d) (get-financial-data :date d)))
   (make-dataset
    :name    "breakdown-trading-data"
    :fetcher (lambda (d) (get-breakdown-trading-data :date d)))
   (make-dataset
    :name    "cash-dividend-data"
    :fetcher (lambda (d) (get-cash-dividend-data :date d)))
   (make-dataset
    :name    "financial-statement-data"
    :fetcher (lambda (d) (get-financial-statement-data :date d)))
   (make-dataset
    :name    "futures-data"
    :fetcher (lambda (d) (get-futures-data :date d)))
   (make-dataset
    :name    "options-data"
    :fetcher (lambda (d) (get-options-data :date d)))))

;;;; ---------- download logic -------------------------------------------------

(defun download-dataset (ds date output-dir)
  "Download a single dataset DS for DATE and write it to OUTPUT-DIR.
Returns :ok on success or (:error condition) on failure."
  (let ((name (dataset-name ds))
        (out  (dataset-path output-dir (dataset-name ds))))
    (format t "[~a] Fetching ~a ...~%" date name)
    (handler-case
        (let ((objects (funcall (dataset-fetcher ds) date)))
          (cond
            ((null objects)
             (format t "[~a] ~a: no data returned (skipped).~%" date name)
             :ok)
            (t
             (save-to-parquet objects (namestring out))
             (format t "[~a] ~a: saved ~a record(s) -> ~a~%"
                     date name (length objects) out)
             :ok)))
      (error (e)
        (format *error-output* "[~a] ~a: ERROR - ~a~%" date name e)
        (list :error e)))))

;;;; ---------- entry point ----------------------------------------------------

(defun main ()
  "Main entry point.  Reads DATE and OUTPUT-DIR from the command-line (or
applies defaults), then downloads all datasets for that date."
  (let* ((argv   (uiop:command-line-arguments))
         (date   (or (first argv) (yesterday)))
         (outdir (or (second argv)
                     (format nil "output/~a/" date))))
    (format t "=== JQuants Daily Download ===~%")
    (format t "Date       : ~a~%" date)
    (format t "Output dir : ~a~%~%" outdir)

    ;; Verify that an API key has been configured.
    (unless *api-key*
      (format *error-output*
              "ERROR: JQUANTS_API_KEY environment variable is not set.~%")
      (uiop:quit 1))

    (ensure-directory outdir)

    (let ((failures '()))
      (dolist (ds (%datasets date))
        (let ((result (download-dataset ds date outdir)))
          (when (and (listp result) (eq (first result) :error))
            (push (dataset-name ds) failures))))

      (cond
        (failures
         (format *error-output*
                 "~%Download completed with ~a failure(s): ~{~a~^, ~}~%"
                 (length failures) failures)
         (uiop:quit 1))
        (t
         (format t "~%Download completed successfully.~%")
         (uiop:quit 0))))))

;; Run when loaded as a --load script (i.e. not inside a REPL).
(unless (find-package :swank)
  (main))
