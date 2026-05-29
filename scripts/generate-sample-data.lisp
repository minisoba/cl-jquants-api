;;;; cl-jquants-api/scripts/generate-sample-data.lisp
;;;;
;;;; Generate a starter set of J-Quants parquet samples for local
;;;; development, using the SAME on-disk layout amx-scheduler produces
;;;; in production:
;;;;
;;;;   <data-dir>/raw/jquants/<endpoint>/date=YYYYMMDD/data.parquet
;;;;
;;;; amx-cache reads from exactly this layout, so the samples this
;;;; script writes are immediately consumable by the cache without any
;;;; config tweaks (point AMX_DATA_TIER / cache.root_dir at <data-dir>).
;;;;
;;;; Usage:
;;;;   export JQUANTS_API_KEY=<your_key>
;;;;
;;;;   # All defaults (snapshot = yesterday, 5 business days of prices):
;;;;   sbcl --non-interactive \
;;;;        --load scripts/generate-sample-data.lisp \
;;;;        --eval '(amx-sample:generate-sample-data :data-dir "/Users/me/amx-data")'
;;;;
;;;;   # Wider history (e.g. 20 days):
;;;;   sbcl --non-interactive \
;;;;        --load scripts/generate-sample-data.lisp \
;;;;        --eval '(amx-sample:generate-sample-data :data-dir "/Users/me/amx-data" :n-days 20)'
;;;;
;;;;   # Pinned snapshot date (for reproducible sample sets):
;;;;   sbcl --non-interactive \
;;;;        --load scripts/generate-sample-data.lisp \
;;;;        --eval '(amx-sample:generate-sample-data :data-dir "/Users/me/amx-data" :snapshot-date "2025-04-30" :n-days 5)'
;;;;
;;;; What gets generated (per call):
;;;;   - listed-issues             : 1 snapshot (the universe)
;;;;   - financial-data            : 1 snapshot
;;;;   - financial-statement-data  : 1 snapshot
;;;;   - stock-prices              : N business days (per-day parquets)
;;;;   - topix-prices              : N business days
;;;;   - indices-prices            : N business days
;;;;   - trading-calendar          : N business days
;;;;
;;;; This is intentionally a small, focused sample. To extend, add more
;;;; DEFENDPOINT forms below or call FETCH-* directly from the REPL.
;;;; Idempotent: existing parquets are skipped (so re-runs only fetch
;;;; what's missing).

(require :asdf)

;; Prefer ql:quickload when available (handles transitive deps cleanly);
;; fall back to asdf:load-system if Quicklisp isn't set up.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((ql-setup (merge-pathnames "quicklisp/setup.lisp"
                                   (user-homedir-pathname))))
    (when (and (probe-file ql-setup)
               (not (find-package "QUICKLISP-CLIENT")))
      (load ql-setup))))

(let ((ql (find-package "QL")))
  (if ql
      (funcall (find-symbol "QUICKLOAD" ql) :cl-jquants-api)
      (asdf:load-system :cl-jquants-api)))

(defpackage :amx-sample
  (:use :cl)
  (:export #:generate-sample-data
           #:generate-sample-data-for-stocks
           #:fetch-listed-issues
           #:fetch-stock-prices
           #:fetch-financial-data
           #:fetch-financial-statement-data
           #:fetch-topix-prices
           #:fetch-indices-prices
           #:fetch-trading-calendar
           #:fetch-cash-dividend-data
           #:fetch-trading-by-type-of-investors
           #:fetch-margin-trading-outstandings
           #:fetch-breakdown-trading-data
           #:fetch-short-sale-value-and-ratio-by-sector
           #:fetch-stock-prices-for-codes
           #:fetch-financial-data-for-codes
           #:fetch-financial-statement-data-for-codes
           #:fetch-cash-dividend-data-for-codes
           #:last-n-business-days))

(in-package :amx-sample)

;;; ---------------------------------------------------------------------------
;;; Setup — API key + locale
;;; ---------------------------------------------------------------------------

(defun ensure-api-key ()
  (let ((k (uiop:getenv "JQUANTS_API_KEY")))
    (unless (and k (> (length k) 0))
      (error "JQUANTS_API_KEY env var must be set (export JQUANTS_API_KEY=<your-key>)"))
    (cl-jquants-api:set-api-key k)
    (setf cl-jquants-api:*locale* :JP)))

;;; ---------------------------------------------------------------------------
;;; Path helpers (mirror amx-scheduler's %jquants-data-set-path layout)
;;; ---------------------------------------------------------------------------

(defun normalize-date (date)
  "YYYY-MM-DD or YYYYMMDD → YYYYMMDD."
  (let ((raw (remove #\- date)))
    (assert (= (length raw) 8) ()
            "bad date format (need YYYY-MM-DD or YYYYMMDD): ~A" date)
    raw))

(defun partition-path (data-dir name date)
  "Hive-partitioned parquet path:
   <data-dir>/raw/jquants/<snake_name>/date=<YYYYMMDD>/data.parquet"
  (let ((dir (merge-pathnames
              (format nil "raw/jquants/~A/date=~A/"
                      (substitute #\_ #\- name)
                      (normalize-date date))
              (uiop:ensure-directory-pathname data-dir))))
    (ensure-directories-exist dir)
    (merge-pathnames (make-pathname :name "data" :type "parquet") dir)))

;;; ---------------------------------------------------------------------------
;;; Per-endpoint fetch-and-save helpers
;;; ---------------------------------------------------------------------------

(defmacro defendpoint (name fetcher-form)
  "Define FETCH-<name> (DATA-DIR DATE) → fetch via FETCHER-FORM
(a closure of one arg DATE), save to the partitioned parquet path.
Idempotent: existing parquets are skipped. Returns the path string."
  (let* ((fn-sym (intern (string-upcase
                          (concatenate 'string "FETCH-" name)))))
    `(defun ,fn-sym (data-dir date)
       (let ((path (partition-path data-dir ,name date)))
         (cond
           ((probe-file path)
            (format t "  [~A] ~A: exists, skipping~%" date ,name)
            (namestring path))
           (t
            (format t "  [~A] ~A: fetching ...~%" date ,name)
            (handler-case
                (let ((objs (funcall ,fetcher-form date)))
                  (cond
                    ((null objs)
                     (format t "  [~A] ~A: no data returned~%" date ,name)
                     nil)
                    (t
                     (cl-jquants-api:save-to-parquet objs (namestring path))
                     (format t "  [~A] ~A: saved ~D record(s) → ~A~%"
                             date ,name (length objs) path)
                     (namestring path))))
              (cl-jquants-api:jquants-error (e)
                (format *error-output*
                        "  [~A] ~A: API error: ~A (skipped)~%"
                        date ,name e)
                nil))))))))

;; Snapshot endpoints (one parquet per snapshot date is plenty)
(defendpoint "listed-issues"
  (lambda (date) (cl-jquants-api:get-listed-issue-master :date date)))

(defendpoint "financial-data"
  (lambda (date) (cl-jquants-api:get-financial-data :date date)))

(defendpoint "financial-statement-data"
  (lambda (date) (cl-jquants-api:get-financial-statement-data :date date)))

(defendpoint "cash-dividend-data"
  (lambda (date) (cl-jquants-api:get-cash-dividend-data :date date)))

;; Daily endpoints (one parquet per business day)
(defendpoint "stock-prices"
  (lambda (date) (cl-jquants-api:get-stock-prices :date date)))

(defendpoint "topix-prices"
  (lambda (date) (cl-jquants-api:get-topix-prices :from date :to date)))

(defendpoint "indices-prices"
  (lambda (date) (cl-jquants-api:get-indices-prices :date date)))

(defendpoint "trading-calendar"
  (lambda (date) (cl-jquants-api:get-trading-calendar :from date :to date)))

(defendpoint "trading-by-type-of-investors"
  (lambda (date) (cl-jquants-api:get-trading-by-type-of-investors :from date :to date)))

(defendpoint "margin-trading-outstandings"
  (lambda (date) (cl-jquants-api:get-margin-trading-outstandings :date date)))

(defendpoint "breakdown-trading-data"
  (lambda (date) (cl-jquants-api:get-breakdown-trading-data :date date)))

(defendpoint "short-sale-value-and-ratio-by-sector"
  (lambda (date) (cl-jquants-api:get-short-sale-value-and-ratio-by-sector :date date)))

;;; ---------------------------------------------------------------------------
;;; Date utility
;;; ---------------------------------------------------------------------------

(defun last-n-business-days (n &optional (from-universal-time (get-universal-time)))
  "Return YYYYMMDD strings for the last N business days, walking back
from yesterday (relative to FROM-UNIVERSAL-TIME)."
  (let ((dates '())
        (d (- from-universal-time 86400)))   ; start from yesterday
    (loop while (< (length dates) n)
          do (multiple-value-bind (s m h day mo yr dow)
                 (decode-universal-time d 0)
               (declare (ignore s m h))
               ;; decode-universal-time dow: 0=Mon, 5=Sat, 6=Sun
               (when (and (/= dow 5) (/= dow 6))
                 (push (format nil "~4,'0D~2,'0D~2,'0D" yr mo day) dates))
               (decf d 86400)))
    (nreverse dates)))

;;; ---------------------------------------------------------------------------
;;; Top-level driver
;;; ---------------------------------------------------------------------------

(defun generate-sample-data (&key data-dir
                                  (n-days 5)
                                  snapshot-date)
  "Generate a starter sample set of J-Quants parquets under DATA-DIR.

  DATA-DIR       Root for the output tree. Required.
  N-DAYS         Business days of stock-prices / topix / indices /
                 trading-calendar to fetch (default 5, walks back from
                 yesterday).
  SNAPSHOT-DATE  Date for the one-shot snapshots (listed-issues,
                 financial-data, financial-statement-data,
                 cash-dividend-data). Default: yesterday. Accepts
                 YYYY-MM-DD or YYYYMMDD.

  Layout written:
    <DATA-DIR>/raw/jquants/listed_issues/date=YYYYMMDD/data.parquet
    <DATA-DIR>/raw/jquants/stock_prices/date=YYYYMMDD/data.parquet
    <DATA-DIR>/raw/jquants/financial_data/date=YYYYMMDD/data.parquet
    ... (etc.)

  Idempotent — re-runs only fetch what's missing on disk."
  (unless data-dir
    (error "data-dir is required"))
  (ensure-api-key)
  (let* ((snap (or snapshot-date (first (last-n-business-days 1))))
         (days (last-n-business-days n-days)))
    (format t "~&== J-Quants sample data ==~%")
    (format t "data-dir       : ~A~%" data-dir)
    (format t "snapshot date  : ~A~%" snap)
    (format t "daily endpoints: ~D business days  (~{~A~^ ~})~%~%"
            (length days) days)

    (format t "-- Snapshot endpoints --~%")
    (fetch-listed-issues             data-dir snap)
    (fetch-financial-data            data-dir snap)
    (fetch-financial-statement-data  data-dir snap)
    (fetch-cash-dividend-data        data-dir snap)

    (format t "~%-- Per-day endpoints (~D days) --~%" (length days))
    (dolist (d days)
      (fetch-stock-prices                       data-dir d)
      (fetch-topix-prices                       data-dir d)
      (fetch-indices-prices                     data-dir d)
      (fetch-trading-calendar                   data-dir d)
      (fetch-trading-by-type-of-investors       data-dir d)
      (fetch-margin-trading-outstandings        data-dir d)
      (fetch-breakdown-trading-data             data-dir d)
      (fetch-short-sale-value-and-ratio-by-sector data-dir d))

    (format t "~%Done. Output tree:~%  ~A~%" data-dir)
    (values)))

;;; ===========================================================================
;;; Per-stock mode — fetch a list of codes over a date range, group
;;; results by date, write the SAME date-partitioned parquet layout the
;;; cache expects.
;;;
;;; Why the grouping step:
;;;   J-Quants's code-mode (:code XXXX :from .. :to ..) returns a stock's
;;;   own time series — rows that span multiple dates. amx-cache reads
;;;   parquets grouped under date=YYYYMMDD/, so after fetching we bucket
;;;   the rows back into per-date parquets (one file per date, possibly
;;;   containing rows for several codes).
;;; ===========================================================================

;; Different J-Quants endpoints expose the "row date" under different
;; slot names. stock-prices / topix-prices / indices-prices / trading-
;; calendar use `date`; financial-data and financial-statement-data use
;; `disclosed-date` (the date the disclosure was published, which is
;; also what amx-scheduler partitions on); cash-dividend-data uses
;; `announcement-date`. Try them in priority order. Values come back as
;; either local-time:timestamp (for `date`) or Unix-epoch integer (for
;; `disclosed-date`/etc.) — handle both.
(defparameter *date-slot-candidates*
  '("date"               ; stock-prices, topix-prices, indices-prices, trading-calendar
    "disclosed-date"     ; financial-data, financial-statement-data
    "announcement-date"  ; cash-dividend-data
    "current-period-end-date")) ; fallback for financial-* if disclosed-date missing

(defun %date-value-to-yyyymmdd (v)
  (cond
    ((null v) nil)
    ((typep v 'local-time:timestamp)
     (local-time:format-timestring
      nil v :format '((:year 4) (:month 2) (:day 2))))
    ((integerp v)
     ;; J-Quants returns Unix epoch seconds for integer-typed date slots.
     (local-time:format-timestring
      nil (local-time:unix-to-timestamp v)
      :format '((:year 4) (:month 2) (:day 2))))
    (t nil)))

(defun %row-date-yyyymmdd (obj)
  "Extract the row's date as YYYYMMDD, by walking class slots and
trying the candidates in *DATE-SLOT-CANDIDATES* order."
  (let* ((class (class-of obj))
         (slot-defs (closer-mop:class-slots class)))
    (dolist (candidate *date-slot-candidates*)
      (let ((match (find candidate slot-defs
                         :key (lambda (s)
                                (string-downcase
                                 (symbol-name
                                  (closer-mop:slot-definition-name s))))
                         :test #'string=)))
        (when match
          (let ((s (%date-value-to-yyyymmdd
                    (slot-value obj (closer-mop:slot-definition-name match)))))
            (when s (return-from %row-date-yyyymmdd s))))))))

;; Merge-with-existing helper. Per-date parquets are shared across ALL
;; stocks that trade that day, so re-running with a different/extended
;; stock list must merge (UNION-dedup) the new rows into the existing
;; file rather than skip — otherwise the new codes' rows are dropped.
(defun %save-or-merge-parquet (new-objects target-path)
  "Write NEW-OBJECTS to TARGET-PATH. If TARGET-PATH already exists,
UNION-dedup with the existing parquet (so re-runs add new codes
without clobbering codes that were already there). Returns (values
ACTION ROW-COUNT) where ACTION is :written, :merged, or :unchanged."
  (cond
    ((null new-objects)
     (values :unchanged 0))
    ((not (probe-file target-path))
     (cl-jquants-api:save-to-parquet new-objects (namestring target-path))
     (values :written (length new-objects)))
    (t
     ;; 1) write new objects to a sibling temp parquet so DuckDB can read it
     (let* ((dir (make-pathname :defaults target-path :name nil :type nil))
            (tmp (merge-pathnames (make-pathname :name "data.new" :type "parquet") dir))
            (merged (merge-pathnames (make-pathname :name "data.merged" :type "parquet") dir)))
       (when (probe-file tmp)    (delete-file tmp))
       (when (probe-file merged) (delete-file merged))
       (cl-jquants-api:save-to-parquet new-objects (namestring tmp))
       ;; 2) UNION-dedup existing + new into a merged parquet
       (duckdb:with-open-database (db)
         (duckdb:with-open-connection (conn db)
           (duckdb:query
             (format nil
                     "COPY (SELECT * FROM read_parquet('~A') ~
                            UNION ~
                            SELECT * FROM read_parquet('~A')) ~
                      TO '~A' (FORMAT PARQUET)"
                     (namestring target-path) (namestring tmp) (namestring merged))
             nil :connection conn)))
       ;; 3) atomically replace existing with merged; clean up temp
       (rename-file merged target-path)
       (delete-file tmp)
       (values :merged (length new-objects))))))

(defun %fetch-per-code-grouped (data-dir endpoint-name codes start-date end-date fetcher)
  "For each CODE in CODES, fetch the endpoint over [START-DATE END-DATE]
via FETCHER (a closure taking (code from to)). Bucket the resulting
rows by date and write one parquet per date under the canonical
hive-partitioned path. If the date partition already exists, MERGE
the new rows in (UNION-dedup) so re-runs with extra stocks add rather
than skip."
  (let ((by-date (make-hash-table :test 'equal))
        (total 0))
    (dolist (code codes)
      (format t "  ~A code=~A [~A..~A]: fetching ...~%"
              endpoint-name code start-date end-date)
      (handler-case
          (let ((rows (funcall fetcher code start-date end-date)))
            (incf total (length rows))
            (dolist (row rows)
              (let ((d (%row-date-yyyymmdd row)))
                (when d (push row (gethash d by-date))))))
        (cl-jquants-api:jquants-error (e)
          (format *error-output*
                  "    code=~A: API error: ~A (skipped)~%" code e))))
    (let ((dates (sort (loop for k being the hash-keys of by-date collect k) #'string<)))
      (format t "  ~A: ~D total rows across ~D dates~%"
              endpoint-name total (length dates))
      (dolist (date dates)
        (let ((path (partition-path data-dir endpoint-name date))
              (rows (gethash date by-date)))
          (multiple-value-bind (action n) (%save-or-merge-parquet rows path)
            (case action
              (:written  (format t "    date=~A: saved   ~D row(s) → ~A~%" date n path))
              (:merged   (format t "    date=~A: merged  ~D new row(s) into ~A~%" date n path))
              (:unchanged (format t "    date=~A: unchanged~%" date)))))))))

(defun fetch-stock-prices-for-codes (data-dir codes start-date end-date)
  (%fetch-per-code-grouped
   data-dir "stock-prices" codes start-date end-date
   (lambda (code from to)
     (cl-jquants-api:get-stock-prices :code code :from from :to to))))

(defun fetch-financial-data-for-codes (data-dir codes start-date end-date)
  (%fetch-per-code-grouped
   data-dir "financial-data" codes start-date end-date
   (lambda (code from to)
     (declare (ignore from to))
     ;; financial-data's code mode returns the full history for the code;
     ;; the grouping step still buckets it correctly by row date.
     (cl-jquants-api:get-financial-data :code code))))

(defun fetch-financial-statement-data-for-codes (data-dir codes start-date end-date)
  (%fetch-per-code-grouped
   data-dir "financial-statement-data" codes start-date end-date
   (lambda (code from to)
     (declare (ignore from to))
     (cl-jquants-api:get-financial-statement-data :code code))))

(defun fetch-cash-dividend-data-for-codes (data-dir codes start-date end-date)
  (%fetch-per-code-grouped
   data-dir "cash-dividend-data" codes start-date end-date
   (lambda (code from to)
     (declare (ignore from to))
     (cl-jquants-api:get-cash-dividend-data :code code))))

(defun %fetch-trading-calendar-grouped (data-dir from to)
  "trading-calendar is market-wide, but the cache still wants per-date
partitions. One :from/:to call, then bucket by date."
  (format t "  trading-calendar [~A..~A]: fetching ...~%" from to)
  (handler-case
      (let ((rows (cl-jquants-api:get-trading-calendar :from from :to to))
            (by-date (make-hash-table :test 'equal)))
        (dolist (r rows)
          (let ((d (%row-date-yyyymmdd r)))
            (when d (push r (gethash d by-date)))))
        (let ((dates (sort (loop for k being the hash-keys of by-date collect k) #'string<)))
          (format t "  trading-calendar: ~D rows across ~D dates~%"
                  (length rows) (length dates))
          (dolist (date dates)
            (let ((path (partition-path data-dir "trading-calendar" date)))
              (unless (probe-file path)
                (cl-jquants-api:save-to-parquet (gethash date by-date)
                                                (namestring path)))))))
    (cl-jquants-api:jquants-error (e)
      (format *error-output* "  trading-calendar: API error: ~A (skipped)~%" e))))

(defun generate-sample-data-for-stocks (&key data-dir codes start-date end-date)
  "Generate sample data for a specific list of stock codes over a date
range. Saves into the canonical date-partitioned parquet layout — same
files amx-cache reads in production.

  DATA-DIR     Root output dir. Required.
  CODES        List of 4-digit JP stock-code strings. Required.
               e.g. '(\"7203\" \"6758\" \"9984\")
  START-DATE   Earliest date (inclusive). YYYY-MM-DD or YYYYMMDD. Required.
  END-DATE     Latest date (inclusive). Default: yesterday.

What's written:
  - listed-issues            : snapshot at END-DATE (full universe — needed
                                for id<->code mapping; cheap, always fetched)
  - stock-prices             : per code, grouped → per-date parquets
  - financial-data           : per code, grouped → per-date parquets
  - financial-statement-data : per code, grouped → per-date parquets
  - cash-dividend-data       : per code, grouped → per-date parquets
  - trading-calendar         : range fetch, grouped → per-date parquets

Idempotent: existing parquets are skipped, so re-runs only fetch what's
missing on disk."
  (unless data-dir   (error ":data-dir is required"))
  (unless codes      (error ":codes is required (a list of 4-digit strings)"))
  (unless start-date (error ":start-date is required (YYYY-MM-DD or YYYYMMDD)"))
  (ensure-api-key)
  (let* ((end (or end-date (first (last-n-business-days 1))))
         (from start-date)
         (to end))
    (format t "~&== J-Quants sample data (per-stock) ==~%")
    (format t "data-dir   : ~A~%" data-dir)
    (format t "codes      : ~{~A~^, ~}~%" codes)
    (format t "date range : ~A .. ~A~%~%" from to)

    (format t "-- Universe snapshot --~%")
    (fetch-listed-issues data-dir end)

    (format t "~%-- Per-code time series (grouped → date partitions) --~%")
    (fetch-stock-prices-for-codes              data-dir codes from to)
    (fetch-financial-data-for-codes            data-dir codes from to)
    (fetch-financial-statement-data-for-codes  data-dir codes from to)
    (fetch-cash-dividend-data-for-codes        data-dir codes from to)

    (format t "~%-- Market-wide (date range) --~%")
    (%fetch-trading-calendar-grouped data-dir from to)

    (format t "~%Done. Output tree: ~A~%" data-dir)
    (values)))
