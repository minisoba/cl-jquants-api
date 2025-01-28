(in-package :cl-jquants-api)

(defun auth-user (auth)
  (let ((auth-data (yason:with-output-to-string* ()
                     (yason:with-object ()
                       (yason:encode-object-element "mailaddress" (auth-mailaddress auth))
                       (yason:encode-object-element "password"    (auth-password auth))))))
    (when-let (hash-table (perform-http-request +token/auth-user+ :method :post :content auth-data))
      (setf *refresh-token* (gethash "refreshToken" hash-table))
      *refresh-token*)))

(defun auth-refresh ()
  (when-let (hash-table (perform-http-request
                         (format nil "~a?refreshtoken=~a" +token/auth-refresh+ *refresh-token*)
                         :method :post))
    (setf *api-token* (gethash "idToken" hash-table))
    *api-token*))

(defun get-listed-issue-master (&key code date)
  (create-jquants-instance
   :class    listed-issue-master
   :node     "info"
   :endpoint +listed/info+
   :params   `((code . ,code) (date . ,date))))

(defun get-stock-prices (&key code date from to)
  (create-jquants-instance
    :class    stock-prices
    :node     "daily_quotes"
    :endpoint +prices/daily-quotes+
    :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
    :alternative-keys '((open . open-price) (high . high-price) (low . low-price) (close . close-price))))

(defun get-indices-prices (&key code date from to)
  (create-jquants-instance
   :class    indices-prices
   :node     "indices"
   :endpoint +indices+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :alternative-keys '((open . open-price) (high . high-price) (low . low-price) (close . close-price))))

(defun get-index-option-prices (&key code date from to)
  (create-jquants-instance
   :class    index-option-prices
   :node     "index_option"
   :endpoint +option/index-option+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :alternative-keys '((open . open-price) (high . high-price) (low . low-price) (close . close-price))))

(defun get-topix-prices (&key from to)
  (create-jquants-instance
   :class    topix-prices
   :node     "topix"
   :endpoint +indices/topix+
   :params   `((from . ,from) (to . ,to))
   :alternative-keys '((open . open-price) (high . high-price) (low . low-price) (close . close-price))))

(defun get-trading-by-type-of-investors (&key section from to)
  (create-jquants-instance
   :class    trading-by-type-of-investors
   :node     "trades_spec"
   :endpoint +markets/trades_spec+
   :params   `((section . ,section) (from . ,from) (to . ,to))
   :alternative-keys '((city-b-ks-regional-b-ks-etc-sales . city-bks-regional-bks-etc-sales)
                       (city-b-ks-regional-b-ks-etc-purchases . city-bks-regional-bks-etc-purchases)
                       (city-b-ks-regional-b-ks-etc-total . city-bks-regional-bks-etc-total)
                       (city-b-ks-regional-b-ks-etc-balance . city-bks-regional-bks-etc-balance))))

(defun get-margin-trading-outstandings (&key code date from to)
  (create-jquants-instance
   :class    margin-trading-outstandings
   :node     "weekly_margin_interest"
   :endpoint +markets/weekly-margin-interest+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))))

(defun get-short-sale-value-and-ratio-by-sector (&key sector33 date from to)
  (create-jquants-instance
   :class    short-sale-value-and-ratio-by-sector
   :node     "short_selling"
   :endpoint +markets/short-selling+
   :params   `((sector33 . ,sector33) (date . ,date) (from . ,from) (to . ,to))))

(defun get-trading-calendar (&key holidaydivision from to)
  (create-jquants-instance
   :class    trading-calendar
   :node     "trading_calendar"
   :endpoint +markets/trading-calendar+
   :params   `((holidaydivision . ,holidaydivision) (from . ,from) (to . ,to))))

(defun get-earnings-calendar ()
  (create-jquants-instance
   :class    earnings-calendar
   :node     "announcement"
   :endpoint +fins/announcement+))

(defun get-financial-data (&key code date)
  (create-jquants-instance
   :class    financial-data
   :node     "statements"
   :endpoint +fins/statements+
   :params   `((code . ,code) (date . ,date))))

(defun get-breakdown-trading-data (&key code date from to)
  (create-jquants-instance
   :class    breakdown-trading-data
   :node     "breakdown"
   :endpoint +markets/breakdown+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))))

(defun get-morning-session-stock-prices (&key code)
  (create-jquants-instance
   :class    morning-session-stock-prices
   :node     "prices_am"
   :endpoint +prices/prices-am+
   :params   `((code . ,code))))

(defun get-cash-dividend-data (&key code date from to)
  (create-jquants-instance
   :class    cash-dividend-data
   :node     "dividend"
   :endpoint +fins/dividend+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))))

(defun get-financial-statement-data (&key code date)
  (create-jquants-instance
   :class    financial-statement-data
   :node     "fs_details"
   :endpoint +fins/fs-details+
   :params   `((code . ,code) (date . ,date))))

(defun get-futures-data (&key category date contract-flag)
  (create-jquants-instance
   :class    futures-data
   :node     "futures"
   :endpoint +derivatives/futures+
   :params   `((category . ,category) (date . ,date) (contract-flag . ,contract-flag))))

(defun get-options-data (&key category date contract-flag)
  (create-jquants-instance
   :class    options-data
   :node     "options"
   :endpoint +derivatives/options+
   :params   `((category . ,category) (date . ,date) (contract-flag . ,contract-flag))))
