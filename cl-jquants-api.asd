(asdf:defsystem :cl-jquants-api
    :author "Y. IGUCHI"
    :license "MIT"
    :description "JPX J-Quants API for Common Lisp"
    :serial t
    :depends-on (:alexandria
                 :cl+ssl
                 :cl-change-case
                 :closer-mop
                 :drakma
                 :fiveam
                 :local-time
                 :parse-number
                 :uiop
                 :yason)
    :components ((:file "src/package")
                 (:file "src/condition")
                 (:file "src/constant")
                 (:file "src/constant-jp")
                 (:file "src/util")
                 (:file "src/http")
                 (:file "src/auth")
                 (:file "src/jquants-object")
                 (:file "src/breakdown-trading-data")
                 (:file "src/cash-dividend-data")
                 (:file "src/earnings-calendar")
                 (:file "src/financial-data")
                 (:file "src/financial-statement-data")
                 (:file "src/futures-data")
                 (:file "src/index-option-prices")
                 (:file "src/indices-prices")
                 (:file "src/listed-issue-master")
                 (:file "src/margin-trading-outstandings")
                 (:file "src/morning-session-stock-prices")
                 (:file "src/options-data")
                 (:file "src/short-sale-value-and-ratio-by-sector")
                 (:file "src/stock-prices")
                 (:file "src/topix-prices")
                 (:file "src/trading-by-type-of-investors")
                 (:file "src/trading-calendar")
                 (:file "src/api"))
    :in-order-to ((asdf:test-op
                   (asdf:test-op :cl-jquants-api-test))))
