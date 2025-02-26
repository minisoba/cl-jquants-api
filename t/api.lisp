(in-package :cl-jquants-api.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+lispworks
  (progn
    (lw:set-default-character-element-type 'cl:character)
    (setf system:*specific-valid-file-encodings* '(:latin-1 :utf-8))))

(def-suite cl-jquants-api-test)

(def-suite* api-test :in cl-jquants-api-test)

(test get-listed-issue-master
  (let* ((all-data (get-listed-issue-master :date 20221111))
         (obj (car all-data)))
    (is (string= (code-of obj) "86970"))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-11-11"))))
    (is (string= (company-name-english-of obj) "Japan Exchange Group,Inc."))
    (is (eq (sector17-code-of obj) (make-keyword "16")))
    (is (eq (sector33-code-of obj) (make-keyword "7200")))
    (is (eq (market-code-of obj) (make-keyword "0111")))
    (is (= (margin-code-of obj) 1))
    (cond ((eq cl-jquants-api:*locale* :jp)
           (is (string= (company-name-of obj) "日本取引所グループ"))
           (is (string= (sector17-code-name-of obj) "金融（除く銀行）"))
           (is (string= (sector33-code-name-of obj) "その他金融業"))
           (is (string= (market-code-name-of obj) "プライム"))
           (is (string= (margin-code-name-of obj) "信用")))
          (t
           (is (string= (company-name-of obj) "Japan Exchange Group,Inc."))
           (is (string= (sector17-code-name-of obj) "FINANCIALS (EX BANKS)"))
           (is (string= (sector33-code-name-of obj) "Other Financing Business"))
           (is (string= (market-code-name-of obj) "Prime"))
           (is (string= (margin-code-name-of obj) "Margin issues"))))))

(test get-stock-prices
  (let* ((all-data (get-stock-prices :date 20230324))
         (obj (car all-data)))                                                               
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-03-24"))))
    (is (string= (code-of obj) "86970"))
    (is (= (open-price-of obj) 2047.0d0))
    (is (= (high-price-of obj) 2069.0d0))
    (is (= (low-price-of obj) 2035.0d0))
    (is (= (close-price-of obj) 2045.0d0))
    (is (= (upper-limit-of obj) 0))
    (is (= (lower-limit-of obj) 0))
    (is (= (volume-of obj) 2202500.0d0))
    (is (= (turnover-value-of obj) 4507051850))
    (is (= (adjustment-factor-of obj) 1.0d0))
    (is (= (adjustment-open-of obj) 2047.0d0))
    (is (= (adjustment-high-of obj) 2069.0d0))
    (is (= (adjustment-low-of obj) 2035.0d0))
    (is (= (adjustment-close-of obj) 2045.0d0))
    (is (= (adjustment-volume-of obj) 2202500))
    (is (= (morning-open-of obj) 2047.0d0))
    (is (= (morning-high-of obj) 2069.0d0))
    (is (= (morning-low-of obj) 2040.0d0))
    (is (= (morning-close-of obj) 2045.5d0))
    (is (= (morning-upper-limit-of obj) 0))
    (is (= (morning-lower-limit-of obj) 0))
    (is (= (morning-volume-of obj) 1121200.0d0))
    (is (= (morning-turnover-value-of obj) 2297525850))
    (is (= (morning-adjustment-open-of obj) 2047.0d0))
    (is (= (morning-adjustment-high-of obj) 2069.0d0))
    (is (= (morning-adjustment-low-of obj) 2040.0d0))
    (is (= (morning-adjustment-close-of obj) 2045.5d0))
    (is (= (morning-adjustment-volume-of obj) 1121200.0d0))
    (is (= (afternoon-open-of obj) 2047.0d0))
    (is (= (afternoon-high-of obj) 2047.0d0))
    (is (= (afternoon-low-of obj) 2035.0d0))
    (is (= (afternoon-close-of obj) 2045.0d0))
    (is (= (afternoon-upper-limit-of obj) 0))
    (is (= (afternoon-lower-limit-of obj) 0))
    (is (= (afternoon-volume-of obj) 1081300.0d0))
    (is (= (afternoon-turnover-value-of obj) 2209526000.0d0))
    (is (= (afternoon-adjustment-open-of obj) 2047.0d0))
    (is (= (afternoon-adjustment-high-of obj) 2047.0d0))
    (is (= (afternoon-adjustment-low-of obj) 2035.0d0))
    (is (= (afternoon-adjustment-close-of obj) 2045.0d0))
    (is (= (afternoon-adjustment-volume-of obj) 1081300.0d0))))

(test get-indices-prices
  (let* ((all-data (get-indices-prices :date 20231201))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-12-01"))))
    (is (eq (code-of obj) (make-keyword "0028")))
    (is (= (open-price-of obj) 1199.18d0))
    (is (= (high-price-of obj) 1202.58d0))
    (is (= (low-price-of obj) 1195.01d0))
    (is (= (close-price-of obj) 1200.17d0))))

(test get-index-option-prices
  (let* ((all-data (get-index-option-prices :date 20230322))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-03-22"))))
    (is (string= (code-of obj) "130060018"))
    (is (= (whole-day-open-of obj) 0.0))
    (is (= (whole-day-high-of obj) 0.0))
    (is (= (whole-day-low-of obj) 0.0))
    (is (= (whole-day-close-of obj) 0.0))
    (is (= (night-session-open-of obj) 0.0))
    (is (= (night-session-high-of obj) 0.0))
    (is (= (night-session-low-of obj) 0.0))
    (is (= (night-session-close-of obj) 0.0))
    (is (= (day-session-open-of obj) 0.0))
    (is (= (day-session-high-of obj) 0.0))
    (is (= (day-session-low-of obj) 0.0))
    (is (= (day-session-close-of obj) 0.0))
    (is (= (volume-of obj) 0.0))
    (is (= (open-interest-of obj) 330.0d0))
    (is (= (turnover-value-of obj) 0.0))
    (is (string= (contract-month-of obj) "2025-06"))
    (is (= (strike-price-of obj) 20000.0d0))
    (is (= (volume-only-auction-of obj) 0.0))
    (is (string= (emergency-margin-trigger-division-of obj) "002"))
    (is (string= (put-call-division-of obj) "1"))
    (is (= (last-trading-day-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2025-06-12"))))
    (is (= (special-quotation-day-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2025-06-13"))))
    (is (= (settlement-price-of obj) 980.0d0))
    (is (= (theoretical-price-of obj) 974.641d0))
    (is (= (base-volatility-of obj) 17.93025d0))
    (is (= (underlying-price-of obj) 27466.61d0))
    (is (= (implied-volatility-of obj) 23.1816d0))
    (is (= (interest-rate-of obj) 0.2336d0))))

(test get-topix-prices
  (let* ((all-data (get-topix-prices :from 20220628))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-06-28"))))
    (is (= (open-price-of obj) 1885.52d0))
    (is (= (high-price-of obj) 1907.38d0))
    (is (= (low-price-of obj) 1885.32d0))
    (is (= (close-price-of obj) 1907.38d0)))
  )

(test get-trading-by-type-of-investors
  (let* ((all-data (get-trading-by-type-of-investors :from 20230130))
         (obj (car all-data)))
    (is (= (published-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2017-01-13"))))
    (is (= (start-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2017-01-04"))))
    (is (= (end-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2017-01-06"))))
    (is (string= (section-of obj) "TSE1st"))
    (is (= (proprietary-sales-of obj) 1311271004.0d0))
    (is (= (proprietary-purchases-of obj) 1453326508.0d0))
    (is (= (proprietary-total-of obj) 2764597512.0d0))
    (is (= (proprietary-balance-of obj) 142055504.0d0))
    (is (= (brokerage-sales-of obj) 7165529005.0d0))
    (is (= (brokerage-purchases-of obj) 7030019854.0d0))
    (is (= (brokerage-total-of obj) 14195548859.0d0))
    (is (= (brokerage-balance-of obj) -135509151.0d0))
    (is (= (total-sales-of obj) 8476800009.0d0))
    (is (= (total-purchases-of obj) 8483346362.0d0))
    (is (= (total-total-of obj) 16960146371.0d0))
    (is (= (total-balance-of obj) 6546353.0d0))
    (is (= (individuals-sales-of obj) 1401711615.0d0))
    (is (= (individuals-purchases-of obj) 1161801155.0d0))
    (is (= (individuals-total-of obj) 2563512770.0d0))
    (is (= (individuals-balance-of obj) -239910460.0d0))
    (is (= (foreigners-sales-of obj) 5094891735.0d0))
    (is (= (foreigners-purchases-of obj) 5317151774.0d0))
    (is (= (foreigners-total-of obj) 10412043509.0d0))
    (is (= (foreigners-balance-of obj) 222260039.0d0))
    (is (= (securities-cos-sales-of obj) 76381455.0d0))
    (is (= (securities-cos-purchases-of obj) 61700100.0d0))
    (is (= (securities-cos-total-of obj) 138081555.0d0))
    (is (= (securities-cos-balance-of obj) -14681355.0d0))
    (is (= (investment-trusts-sales-of obj) 168705109.0d0))
    (is (= (investment-trusts-purchases-of obj) 124389642.0d0))
    (is (= (investment-trusts-total-of obj) 293094751.0d0))
    (is (= (investment-trusts-balance-of obj) -44315467.0d0))
    (is (= (business-cos-sales-of obj) 71217959.0d0))
    (is (= (business-cos-purchases-of obj) 63526641.0d0))
    (is (= (business-cos-total-of obj) 134744600.0d0))
    (is (= (business-cos-balance-of obj) -7691318.0d0))
    (is (= (other-cos-sales-of obj) 10745152.0d0))
    (is (= (other-cos-purchases-of obj) 15687836.0d0))
    (is (= (other-cos-total-of obj) 26432988.0d0))
    (is (= (other-cos-balance-of obj) 4942684.0d0))
    (is (= (insurance-cos-sales-of obj) 15926202.0d0))
    (is (= (insurance-cos-purchases-of obj) 9831555.0d0))
    (is (= (insurance-cos-total-of obj) 25757757.0d0))
    (is (= (insurance-cos-balance-of obj) -6094647.0d0))
    (is (= (city-bks-regional-bks-etc-sales-of obj) 10606789.0d0))
    (is (= (city-bks-regional-bks-etc-purchases-of obj) 8843871.0d0))
    (is (= (city-bks-regional-bks-etc-total-of obj) 19450660.0d0))
    (is (= (city-bks-regional-bks-etc-balance-of obj) -1762918.0d0))
    (is (= (trust-banks-sales-of obj) 292932297.0d0))
    (is (= (trust-banks-purchases-of obj) 245322795.0d0))
    (is (= (trust-banks-total-of obj) 538255092.0d0))
    (is (= (trust-banks-balance-of obj) -47609502.0d0))
    (is (= (other-financial-institutions-sales-of obj) 22410692.0d0))
    (is (= (other-financial-institutions-purchases-of obj) 21764485.0d0))
    (is (= (other-financial-institutions-total-of obj) 44175177.0d0))
    (is (= (other-financial-institutions-balance-of obj) -646207.0d0))))

(test get-margin-trading-outstandings
  (let* ((all-data (get-margin-trading-outstandings :date 20230217))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-02-17"))))
    (is (string= (code-of obj) "13010"))
    (is (= (short-margin-trade-volume-of obj) 4100.0d0))
    (is (= (long-margin-trade-volume-of obj) 27600.0d0))
    (is (= (short-negotiable-margin-trade-volume-of obj) 1300.0d0))
    (is (= (long-negotiable-margin-trade-volume-of obj) 7600.0d0))
    (is (= (short-standardized-margin-trade-volume-of obj) 2800.0d0))
    (is (= (long-standardized-margin-trade-volume-of obj) 20000.0d0))
    (is (= (issue-type-of obj) 2))))

(test get-short-sale-value-and-ratio-by-sector
  (let* ((all-data (get-short-sale-value-and-ratio-by-sector :date 20221025))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-10-25"))))
    (is (eq (sector33-code-of obj) :0050))
    (is (= (selling-excluding-short-selling-turnover-value-of obj) 1333126400.0d0))
    (is (= (short-selling-with-restrictions-turnover-value-of obj) 787355200.0d0))
    (is (= (short-selling-without-restrictions-turnover-value-of obj) 149084300.0d0))))

(test get-trading-calendar
  (let* ((all-data (get-trading-calendar))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2015-04-01"))))
    (is (= (holiday-division-of obj) 1))))

(test get-earnings-calendar
  (let* ((all-data (get-earnings-calendar))
         (obj (car all-data)))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-02-14"))))
    (is (string= (code-of obj) "43760"))
    (when (eq cl-jquants-api:*locale* :jp)
      (is (string= (company-name-of obj) "くふうカンパニー"))
      (is (string= (fiscal-year-of obj) "9月30日"))
      (is (string= (sector-name-of obj) "情報・通信業"))
      (is (string= (fiscal-quarter-of obj) "第１四半期"))
      (is (string= (section-of obj) "マザーズ")))))

(test get-financial-data
  (let* ((all-data (get-financial-data :date 20230130))
         (obj (car all-data)))
    (is (= (disclosed-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-01-30"))))
    (is (= (disclosed-time-of obj) 43200))
    (is (string= (local-code-of obj) "86970"))
    (is (string= (disclosure-number-of obj) "20230127594871"))
    (is (string= (type-of-document-of obj) "3QFinancialStatements_Consolidated_IFRS"))
    (is (string= (type-of-current-period-of obj) "3Q"))
    (is (= (current-period-start-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-04-01"))))
    (is (= (current-period-end-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-12-31"))))
    (is (= (current-fiscal-year-start-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-04-01"))))
    (is (= (current-fiscal-year-end-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-03-31"))))
    (is (eq (next-fiscal-year-start-date-of obj) nil))
    (is (eq (next-fiscal-year-end-date-of obj) nil))
    (is (= (net-sales-of obj) 100529000000))
    (is (= (operating-profit-of obj) 51765000000))
    (is (eq (ordinary-profit-of obj) nil))
    (is (= (profit-of obj) 35175000000))
    (is (= (earnings-per-share-of obj) 66.76d0))
    (is (eq (diluted-earnings-per-share-of obj) nil))
    (is (= (total-assets-of obj) 79205861000000))
    (is (= (equity-of obj) 320021000000))
    (is (= (equity-to-asset-ratio-of obj) 0.004d0))
    (is (eq (book-value-per-share-of obj) nil))
    (is (eq (cash-flows-from-operating-activities-of obj) nil))
    (is (eq (cash-flows-from-investing-activities-of obj) nil))
    (is (eq (cash-flows-from-financing-activities-of obj) nil))
    (is (= (cash-and-equivalents-of obj) 91135000000.0d0))
    (is (eq (result-dividend-per-share1st-quarter-of obj) nil))
    (is (= (result-dividend-per-share2nd-quarter-of obj) 26.0d0))
    (is (eq (result-dividend-per-share3rd-quarter-of obj) nil))
    (is (eq (result-dividend-per-share-fiscal-year-end-of obj) nil))
    (is (eq (result-dividend-per-share-annual-of obj) nil))
    (is (eq (distributions-per-unit-reit-of obj) nil))
    (is (eq (result-total-dividend-paid-annual-of obj) nil))
    (is (eq (result-payout-ratio-annual-of obj) nil))
    (is (eq (forecast-dividend-per-share1st-quarter-of obj) nil))
    (is (eq (forecast-dividend-per-share2nd-quarter-of obj) nil))
    (is (eq (forecast-dividend-per-share3rd-quarter-of obj) nil))
    (is (= (forecast-dividend-per-share-fiscal-year-end-of obj) 36.0d0))
    (is (= (forecast-dividend-per-share-annual-of obj) 62.0d0))
    (is (eq (forecast-distributions-per-unit-reit-of obj) nil))
    (is (eq (forecast-total-dividend-paid-annual-of obj) nil))
    (is (eq (forecast-payout-ratio-annual-of obj) nil))
    (is (eq (next-year-forecast-dividend-per-share1st-quarter-of obj) nil))
    (is (eq (next-year-forecast-dividend-per-share2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-dividend-per-share3rd-quarter-of obj) nil))
    (is (eq (next-year-forecast-dividend-per-share-fiscal-year-end-of obj) nil))
    (is (eq (next-year-forecast-dividend-per-share-annual-of obj) nil))
    (is (eq (next-year-forecast-distributions-per-unit-reit-of obj) nil))
    (is (eq (next-year-forecast-payout-ratio-annual-of obj) nil))
    (is (eq (forecast-net-sales2nd-quarter-of obj) nil))
    (is (eq (forecast-operating-profit2nd-quarter-of obj) nil))
    (is (eq (forecast-ordinary-profit2nd-quarter-of obj) nil))
    (is (eq (forecast-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-earnings-per-share2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-net-sales2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-operating-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-ordinary-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-earnings-per-share2nd-quarter-of obj) nil))
    (is (= (forecast-net-sales-of obj) 132500000000.0d0))
    (is (= (forecast-operating-profit-of obj) 65500000000.0d0))
    (is (eq (forecast-ordinary-profit-of obj) nil))
    (is (= (forecast-profit-of obj) 45000000000.0d0))
    (is (= (forecast-earnings-per-share-of obj) 85.42d0))
    (is (eq (next-year-forecast-net-sales-of obj) nil))
    (is (eq (next-year-forecast-operating-profit-of obj) nil))
    (is (eq (next-year-forecast-ordinary-profit-of obj) nil))
    (is (eq (next-year-forecast-profit-of obj) nil))
    (is (eq (next-year-forecast-earnings-per-share-of obj) nil))
    (is (eq (material-changes-in-subsidiaries-of obj) nil))
    (is (eq (significant-changes-in-the-scope-of-consolidation-of obj) nil))
    (is (eq (changes-based-on-revisions-of-accounting-standard-of obj) nil))
    (is (eq (changes-other-than-ones-based-on-revisions-of-accounting-standard-of obj) nil))
    (is (eq (changes-in-accounting-estimates-of obj) t))
    (is (eq (retrospective-restatement-of obj) nil))
    (is (= (number-of-issued-and-outstanding-shares-at-the-end-of-fiscal-year-including-treasury-stock-of obj) 528578441.0d0))
    (is (= (number-of-treasury-stock-at-the-end-of-fiscal-year-of obj) 1861043.0d0))
    (is (= (average-number-of-shares-of obj) 526874759.0d0))
    (is (eq (non-consolidated-net-sales-of obj) nil))
    (is (eq (non-consolidated-operating-profit-of obj) nil))
    (is (eq (non-consolidated-ordinary-profit-of obj) nil))
    (is (eq (non-consolidated-profit-of obj) nil))
    (is (eq (non-consolidated-earnings-per-share-of obj) nil))
    (is (eq (non-consolidated-total-assets-of obj) nil))
    (is (eq (non-consolidated-equity-of obj) nil))
    (is (eq (non-consolidated-equity-to-asset-ratio-of obj) nil))
    (is (eq (non-consolidated-book-value-per-share-of obj) nil))
    (is (eq (forecast-non-consolidated-net-sales-of obj) nil))
    (is (eq (forecast-non-consolidated-operating-profit2nd-quarter-of obj) nil))
    (is (eq (forecast-non-consolidated-ordinary-profit2nd-quarter-of obj) nil))
    (is (eq (forecast-non-consolidated-profit2nd-quarter-of obj) nil))
    (is (eq (forecast-non-consolidated-earnings-per-share2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-net-sales2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-operating-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-ordinary-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-profit2nd-quarter-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-earnings-per-share2nd-quarter-of obj) nil))
    (is (eq (forecast-non-consolidated-net-sales-of obj) nil))
    (is (eq (forecast-non-consolidated-operating-profit-of obj) nil))
    (is (eq (forecast-non-consolidated-ordinary-profit-of obj) nil))
    (is (eq (forecast-non-consolidated-profit-of obj) nil))
    (is (eq (forecast-non-consolidated-earnings-per-share-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-net-sales-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-operating-profit-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-ordinary-profit-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-profit-of obj) nil))
    (is (eq (next-year-forecast-non-consolidated-earnings-per-share-of obj) nil))))

(test get-breakdown-trading-data
  (let* ((all-data (get-breakdown-trading-data :date 20150401))
         (obj (car all-data)))
    (is (string= (code-of obj) "13010"))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2015-04-01"))))
    (is (= (long-sell-value-of obj) 115164000.0d0))
    (is (= (short-sell-without-margin-value-of obj) 93561000.0d0))
    (is (= (margin-sell-new-value-of obj) 6412000.0d0))
    (is (= (margin-sell-close-value-of obj) 23009000.0d0))
    (is (= (long-buy-value-of obj) 185114000.0d0))
    (is (= (margin-buy-new-value-of obj) 35568000.0d0))
    (is (= (margin-buy-close-value-of obj) 17464000.0d0))
    (is (= (long-sell-volume-of obj) 415000.0d0))
    (is (= (short-sell-without-margin-volume-of obj) 337000.0d0))
    (is (= (margin-sell-new-volume-of obj) 23000.0d0))
    (is (= (margin-sell-close-volume-of obj) 83000.0d0))
    (is (= (long-buy-volume-of obj) 667000.0d0))
    (is (= (margin-buy-new-volume-of obj) 128000.0d0))
    (is (= (margin-buy-close-volume-of obj) 63000))))

(test get-morning-session-stock-prices
  (let* ((all-data (get-morning-session-stock-prices :code 3940))
         (obj (car all-data)))
    (is (string= (code-of obj) "39400"))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-03-20"))))
    (is (= (morning-open-of obj) 232.0d0))
    (is (= (morning-high-of obj) 244.0d0))
    (is (= (morning-low-of obj) 232.0d0))
    (is (= (morning-close-of obj) 240.0d0))
    (is (= (morning-volume-of obj) 52600.0d0))
    (is (= (morning-turnover-value-of obj) 12518800))))

(test get-cash-dividend-data
  (let* ((all-data (get-cash-dividend-data :code 1555))
         (obj (car all-data)))
    (is (string= (code-of obj) "15550"))
    (is (= (announcement-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2014-02-24"))))
    (is (= (announcement-time-of obj) 33660))
    (is (string= (reference-number-of obj) "201402241B00002"))
    (is (= (status-code-of obj) 1))
    (is (= (board-meeting-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2014-02-24"))))
    (is (= (interim-final-code-of obj) 2))
    (is (= (forecast-result-code-of obj) 2))
    (is (string= (interim-final-term-of obj) "2014-03"))
    (is (eq (gross-dividend-rate-of obj) :undetermined))
    (is (= (record-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2014-03-10"))))
    (is (= (ex-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2014-03-06"))))
    (is (= (actual-record-date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2014-03-10"))))
    (is (eq (payable-date-of obj) :undetermined))
    (is (string= (ca-reference-number-of obj) "201402241B00002"))
    (is (eq (distribution-amount-of obj) nil))
    (is (eq (retained-earnings-of obj) nil))
    (is (eq (deemed-dividend-of obj) nil))
    (is (eq (deemed-capital-gains-of obj) nil))
    (is (eq (net-asset-decrease-ratio-of obj) nil))
    (is (= (commemorative-special-code-of obj) 0))
    (is (eq (commemorative-dividend-rate-of obj) :not-applicable))
    (is (eq (special-dividend-rate-of obj) nil))))

(test get-financial-statement-data
  (let* ((all-data (get-financial-statement-data :date 20230130))
         (obj (car all-data))
         (fs (financial-statement-of obj)))
    (is (= (disclosed-time-of obj) 43200))
    (is (string= (local-code-of obj) "86970"))
    (is (string= (disclosure-number-of obj) "20230127594871"))
    (is (string= (type-of-document-of obj) "3QFinancialStatements_Consolidated_IFRS"))
    (is (= (goodwill-ifrs-of fs) 67374000000.0d0))
    (is (= (retained-earnings-ifrs-of fs) 263894000000.0d0))
    (is (= (operating-profit-loss-ifrs-of fs) 51765000000.0d0))
    (is (= (previous-fiscal-year-end-date-dei-of fs)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-03-31"))))
    (is (= (basic-earnings-loss-per-share-ifrs-of fs) 66.76d0))
    (is (= (current-period-end-date-dei-of fs)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-12-31"))))
    (is (= (revenue-2-ifrs-of fs) 100987000000.0d0))
    (is (string= (industry-code-when-consolidated-financial-statements-are-prepared-in-accordance-with-industry-specific-regulations-dei-of fs) "CTE"))
    (is (= (profit-loss-attributable-to-owners-of-parent-ifrs-of fs) 35175000000.0d0))
    (is (= (other-current-liabilities-cl-ifrs-of fs) 8904000000.0d0))
    (is (= (share-of-profit-loss-of-investments-accounted-for-using-equity-method-ifrs-of fs) 1042000000.0d0))
    (is (= (current-liabilities-ifrs-of fs) 78852363000000.0d0))
    (is (= (equity-attributable-to-owners-of-parent-ifrs-of fs) 311103000000.0d0))
    (is (eq (whether-consolidated-financial-statements-are-prepared-dei-of fs) t))
    (is (= (non-current-liabilities-ifrs-of fs) 33476000000.0d0))
    (is (= (other-expenses-ifrs-of fs) 58000000.0d0))
    (is (= (income-taxes-payable-cl-ifrs-of fs) 5245000000.0d0))
    (is (string= (filer-name-in-english-dei-of fs) "Japan Exchange Group, Inc."))
    (is (= (non-controlling-interests-ifrs-of fs) 8918000000.0d0))
    (is (= (capital-surplus-ifrs-of fs) 38844000000.0d0))
    (is (= (finance-costs-ifrs-of fs) 71000000.0d0))
    (is (= (other-current-assets-ca-ifrs-of fs) 4217000000.0d0))
    (is (= (property-plant-and-equipment-ifrs-of fs) 11277000000.0d0))
    (is (= (deferred-tax-liabilities-ifrs-of fs) 419000000.0d0))
    (is (= (other-components-of-equity-ifrs-of fs) 422000000.0d0))
    (is (= (current-fiscal-year-start-date-dei-of fs)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2022-04-01"))))
    (is (string= (type-of-current-period-dei-of fs) "Q3"))
    (is (= (cash-and-cash-equivalents-ifrs-of fs) 91135000000.0d0))
    (is (= (share-capital-ifrs-of fs) 11500000000.0d0))
    (is (= (retirement-benefit-asset-nca-ifrs-of fs) 9028000000.0d0))
    (is (= (number-of-submission-dei-of fs) 1))
    (is (= (trade-and-other-receivables-ca-ifrs-of fs) 18837000000.0d0))
    (is (= (liabilities-and-equity-ifrs-of fs) 79205861000000.0d0))
    (is (string= (edinet-code-dei-of fs) "E03814"))
    (is (= (equity-ifrs-of fs) 320021000000.0d0))
    (is (string= (security-code-dei-of fs) "86970"))
    (is (= (other-financial-assets-ca-ifrs-of fs) 112400000000.0d0))
    (is (= (other-financial-assets-nca-ifrs-of fs) 2898000000.0d0))
    (is (= (income-taxes-receivable-ca-ifrs-of fs) 5529000000.0d0))
    (is (= (investments-accounted-for-using-equity-method-ifrs-of fs) 18362000000.0d0))
    (is (= (other-non-current-assets-nca-ifrs-of fs) 6240000000.0d0))
    (is (= (previous-fiscal-year-start-date-dei-of fs)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2021-04-01"))))
    (is (= (deferred-tax-assets-ifrs-of fs) 2862000000.0d0))
    (is (= (trade-and-other-payables-cl-ifrs-of fs) 5037000000.0d0))
    (is (= (bonds-and-borrowings-cl-ifrs-of fs) 33000000000.0d0))
    (is (= (current-fiscal-year-end-date-dei-of fs)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2023-03-31"))))
    (is (eq (xbrl-amendment-flag-dei-of fs) nil))
    (is (= (non-current-assets-ifrs-of fs) 182317000000.0d0))
    (is (= (retirement-benefit-liability-ncl-ifrs-of fs) 9214000000.0d0))
    (is (eq (amendment-flag-dei-of fs) nil))
    (is (= (assets-ifrs-of fs) 79205861000000.0d0))
    (is (= (income-tax-expense-ifrs-of fs) 15841000000.0d0))
    (is (eq (report-amendment-flag-dei-of fs) nil))
    (is (= (profit-loss-ifrs-of fs) 35894000000.0d0))
    (is (= (operating-expenses-ifrs-of fs) 50206000000.0d0))
    (is (= (intangible-assets-ifrs-of fs) 36324000000))
    (is (= (profit-loss-before-tax-from-continuing-operations-ifrs-of fs) 51736000000.0d0))
    (is (= (liabilities-ifrs-of fs) 78885839000000.0d0))
    (is (string= (accounting-standards-dei-of fs) "IFRS"))
    (is (= (bonds-and-borrowings-ncl-ifrs-of fs) 19972000000.0d0))
    (is (= (finance-income-ifrs-of fs) 43000000.0d0))
    (is (= (profit-loss-attributable-to-non-controlling-interests-ifrs-of fs) 719000000.0d0))
    (is (= (comparative-period-end-date-dei-of fs)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2021-12-31"))))
    (is (= (current-assets-ifrs-of fs) 79023543000000.0d0))
    (is (= (other-non-current-liabilities-ncl-ifrs-of fs) 3870000000.0d0))
    (is (= (other-income-ifrs-of fs) 458000000.0d0))
    (is (= (treasury-shares-ifrs-of fs) -3556000000.0d0))
    (when (eq cl-jquants-api:*locale* :jp)
      (is (string= (document-type-dei-of fs) "四半期第３号参考様式　[IFRS]（連結）"))
      (is (string= (filer-name-in-japanese-dei-of fs) "株式会社日本取引所グループ")))))

(test get-futures-data
  (let* ((all-data (get-futures-data :date 20240723))
         (obj (car all-data)))
    (is (string= (code-of obj) "169090005"))
    (is (string= (derivatives-product-category-of obj) "TOPIXF"))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2024-07-23"))))
    (is (= (whole-day-open-of obj) 2825.5d0))
    (is (= (whole-day-high-of obj) 2853.0d0))
    (is (= (whole-day-low-of obj) 2825.5d0))
    (is (= (whole-day-close-of obj) 2829.0d0)
    (is (eq (morning-session-open-of obj) nil))
    (is (eq (morning-session-high-of obj) nil))
    (is (eq (morning-session-low-of obj) nil))
    (is (eq (morning-session-close-of obj) nil))
    (is (= (night-session-open-of obj) 2825.5d0))
    (is (= (night-session-high-of obj) 2850.0d0))
    (is (= (night-session-low-of obj) 2825.5d0))
    (is (= (night-session-close-of obj) 2845.0d0))
    (is (= (day-session-open-of obj) 2850.5d0))
    (is (= (day-session-high-of obj) 2853.0d0))
    (is (= (day-session-low-of obj) 2826.0d0))
    (is (= (day-session-close-of obj) 2829.0d0))
    (is (= (volume-of obj) 3210.0d0))
    (is (= (open-interest-of obj) 479812.0d0))
    (is (= (turnover-value-of obj) 1217918971856.0d0))
    (is (string= (contract-month-of obj) "2024-09"))
    (is (= (volume-only-auction-of obj) 40405.0d0))
    (is (string= (emergency-margin-trigger-division-of obj) "002"))
    (is (= (last-trading-day-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2024-09-12"))))
    (is (= (special-quotation-day-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2024-09-13"))))
    (is (= (settlement-price-of obj) 2829.0d0))
    (is (string= (central-contract-month-flag-of obj) :others)))))

(test get-options-data
  (let* ((all-data (get-options-data :date 20240723))
         (obj (car all-data)))
    (is (string= (code-of obj) "140014505"))
    (is (string= (derivatives-product-category-of obj) "TOPIXE"))
    (is (string= (underlying-sso-of obj) "-"))
    (is (= (date-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2024-07-23"))))
    (is (= (whole-day-open-of obj) 0.0))
    (is (= (whole-day-high-of obj) 0.0))
    (is (= (whole-day-low-of obj) 0.0))
    (is (= (whole-day-close-of obj) 0.0))
    (is (eq (morning-session-open-of obj) nil))
    (is (eq (morning-session-high-of obj) nil))
    (is (eq (morning-session-low-of obj) nil))
    (is (eq (morning-session-close-of obj) nil))
    (is (= (night-session-open-of obj) 0.0))
    (is (= (night-session-high-of obj) 0.0))
    (is (= (night-session-low-of obj) 0.0))
    (is (= (night-session-close-of obj) 0.0))
    (is (= (day-session-open-of obj) 0.0))
    (is (= (day-session-high-of obj) 0.0))
    (is (= (day-session-low-of obj) 0.0))
    (is (= (day-session-close-of obj) 0.0))
    (is (= (volume-of obj) 0.0))
    (is (= (open-interest-of obj) 0.0))
    (is (= (turnover-value-of obj) 0.0))
    (is (string= (contract-month-of obj) "2025-01"))
    (is (= (strike-price-of obj) 2450.0d0))
    (is (= (volume-only-auction-of obj) 0.0))
    (is (string= (emergency-margin-trigger-division-of obj) "002"))
    (is (string= (put-call-division-of obj) :call))
    (is (= (last-trading-day-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2025-01-09"))))
    (is (= (special-quotation-day-of obj)
           (local-time:timestamp-to-unix
            (local-time:parse-timestring "2025-01-10"))))
    (is (= (settlement-price-of obj) 377.0d0))
    (is (= (theoretical-price-of obj) 380.3801d0))
    (is (= (base-volatility-of obj) 18.115d0))
    (is (= (underlying-price-of obj) 2833.39d0))
    (is (= (implied-volatility-of obj) 17.2955d0))
    (is (= (interest-rate-of obj) 0.3527d0))
    (is (string= (central-contract-month-flag-of obj) :others))))

(defun run-test ()
  (setf *read-default-float-format* 'double-float)
  (setf jquants::*http-handler* (make-instance 'mock-http-handler))
  (run! 'cl-jquants-api-test))
