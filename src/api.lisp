(in-package :cl-jquants-api)

(defun get-listed-issue-master (&key code date)
  "Listed-issue master records (codes, names, 17/33 sector, market segment, margin class) from JPX /equities/master."
  (create-jquants-instance
   :class    listed-issue-master
   :node     "data"
   :endpoint +equities/master+
   :params   `((code . ,code) (date . ,date))
   :key-map '(("CoName" . company-name)
              ("CoNameEn" . company-name-english)
              ("S17" . sector17-code)
              ("S17Nm" . sector17-code-name)
              ("S33" . sector33-code)
              ("S33Nm" . sector33-code-name)
              ("ScaleCat" . scale-category)
              ("Mkt" . market-code)
              ("MktNm" . market-code-name)
              ("Mrgn" . margin-code)
              ("MrgnNm" . margin-code-name)
              ("ProdCat" . product-category))))

(defun get-stock-prices (&key code date from to)
  "Daily OHLCV stock prices (with adjustment factors and morning/afternoon-session breakdowns) from JPX /equities/bars/daily."
  (create-jquants-instance
   :class    stock-prices
   :node     "data"
   :endpoint +equities/bars/daily+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("O" . open-price) ("H" . high-price) ("L" . low-price) ("C" . close-price)
              ("UL" . upper-limit) ("LL" . lower-limit) ("Vo" . volume) ("Va" . turnover-value)
              ("AdjFactor" . adjustment-factor) ("AdjO" . adjustment-open) ("AdjH" . adjustment-high)
              ("AdjL" . adjustment-low) ("AdjC" . adjustment-close) ("AdjVo" . adjustment-volume)
              ("MO" . morning-open) ("MH" . morning-high) ("ML" . morning-low) ("MC" . morning-close)
              ("MUL" . morning-upper-limit) ("MLL" . morning-lower-limit)
              ("MVo" . morning-volume) ("MVa" . morning-turnover-value)
              ("MAdjO" . morning-adjustment-open) ("MAdjH" . morning-adjustment-high)
              ("MAdjL" . morning-adjustment-low) ("MAdjC" . morning-adjustment-close)
              ("MAdjVo" . morning-adjustment-volume)
              ("AO" . afternoon-open) ("AH" . afternoon-high) ("AL" . afternoon-low) ("AC" . afternoon-close)
              ("AUL" . afternoon-upper-limit) ("ALL" . afternoon-lower-limit)
              ("AVo" . afternoon-volume) ("AVa" . afternoon-turnover-value)
              ("AAdjO" . afternoon-adjustment-open) ("AAdjH" . afternoon-adjustment-high)
              ("AAdjL" . afternoon-adjustment-low) ("AAdjC" . afternoon-adjustment-close)
              ("AAdjVo" . afternoon-adjustment-volume))))

(defun get-indices-prices (&key code date from to)
  "Daily OHLC for JPX stock-market indices (TOPIX sub-indices, JPX-Nikkei 400, sector indices, etc.) from /indices/bars/daily."
  (create-jquants-instance
   :class    indices-prices
   :node     "data"
   :endpoint +indices/bars/daily+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("O" . open-price) ("H" . high-price) ("L" . low-price) ("C" . close-price))))

(defun get-index-option-prices (&key code date from to)
  "Daily Nikkei 225 index option prices (whole-day / night-session / day-session OHLC, settlement, IV, theoretical price) from /derivatives/bars/daily/options/225."
  (create-jquants-instance
   :class    index-option-prices
   :node     "data"
   :endpoint +derivatives/bars/daily/options/225+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("O" . whole-day-open) ("H" . whole-day-high) ("L" . whole-day-low) ("C" . whole-day-close)
              ("EO" . night-session-open) ("EH" . night-session-high) ("EL" . night-session-low) ("EC" . night-session-close)
              ("AO" . day-session-open) ("AH" . day-session-high) ("AL" . day-session-low) ("AC" . day-session-close)
              ("Vo" . volume) ("OI" . open-interest) ("Va" . turnover-value) ("CM" . contract-month)
              ("Strike" . strike-price) ("VoOA" . volume-only-auction) ("EmMrgnTrgDiv" . emergency-margin-trigger-division)
              ("PCDiv" . put-call-division) ("LTD" . last-trading-day) ("SQD" . special-quotation-day)
              ("Settle" . settlement-price) ("Theo" . theoretical-price) ("BaseVol" . base-volatility)
              ("UnderPx" . underlying-price) ("IV" . implied-volatility) ("IR" . interest-rate))))

(defun get-topix-prices (&key from to)
  "Daily TOPIX (Tokyo Stock Price Index) OHLC from JPX /indices/bars/daily/topix."
  (create-jquants-instance
   :class    topix-prices
   :node     "data"
   :endpoint +indices/bars/daily/topix+
   :params   `((from . ,from) (to . ,to))
   :key-map '(("O" . open-price) ("H" . high-price) ("L" . low-price) ("C" . close-price))))

(defun get-trading-by-type-of-investors (&key section from to)
  "Weekly buy / sell / balance values broken down by investor category (proprietary, brokerage, foreigners, trust banks, etc.) from JPX /equities/investor-types."
  (create-jquants-instance
   :class    trading-by-type-of-investors
   :node     "data"
   :endpoint +equities/investor-types+
   :params   `((section . ,section) (from . ,from) (to . ,to))
   :key-map '(("PubDate" . published-date) ("StDate" . start-date) ("EnDate" . end-date)
              ("PropSell" . proprietary-sales) ("PropBuy" . proprietary-purchases) ("PropTot" . proprietary-total) ("PropBal" . proprietary-balance)
              ("BrkSell" . brokerage-sales) ("BrkBuy" . brokerage-purchases) ("BrkTot" . brokerage-total) ("BrkBal" . brokerage-balance)
              ("TotSell" . total-sales) ("TotBuy" . total-purchases) ("TotTot" . total-total) ("TotBal" . total-balance)
              ("IndSell" . individuals-sales) ("IndBuy" . individuals-purchases) ("IndTot" . individuals-total) ("IndBal" . individuals-balance)
              ("FrgnSell" . foreigners-sales) ("FrgnBuy" . foreigners-purchases) ("FrgnTot" . foreigners-total) ("FrgnBal" . foreigners-balance)
              ("SecCoSell" . securities-cos-sales) ("SecCoBuy" . securities-cos-purchases) ("SecCoTot" . securities-cos-total) ("SecCoBal" . securities-cos-balance)
              ("InvTrSell" . investment-trusts-sales) ("InvTrBuy" . investment-trusts-purchases) ("InvTrTot" . investment-trusts-total) ("InvTrBal" . investment-trusts-balance)
              ("BusCoSell" . business-cos-sales) ("BusCoBuy" . business-cos-purchases) ("BusCoTot" . business-cos-total) ("BusCoBal" . business-cos-balance)
              ("OthCoSell" . other-cos-sales) ("OthCoBuy" . other-cos-purchases) ("OthCoTot" . other-cos-total) ("OthCoBal" . other-cos-balance)
              ("InsCoSell" . insurance-cos-sales) ("InsCoBuy" . insurance-cos-purchases) ("InsCoTot" . insurance-cos-total) ("InsCoBal" . insurance-cos-balance)
              ("BankSell" . city-bks-regional-bks-etc-sales) ("BankBuy" . city-bks-regional-bks-etc-purchases) ("BankTot" . city-bks-regional-bks-etc-total) ("BankBal" . city-bks-regional-bks-etc-balance)
              ("TrstBnkSell" . trust-banks-sales) ("TrstBnkBuy" . trust-banks-purchases) ("TrstBnkTot" . trust-banks-total) ("TrstBnkBal" . trust-banks-balance)
              ("OthFinSell" . other-financial-institutions-sales) ("OthFinBuy" . other-financial-institutions-purchases) ("OthFinTot" . other-financial-institutions-total) ("OthFinBal" . other-financial-institutions-balance))))

(defun get-margin-trading-outstandings (&key code date from to)
  "Weekly margin-trading outstandings — long / short balances split across negotiable and standardized margin — from JPX /markets/margin-interest."
  (create-jquants-instance
   :class    margin-trading-outstandings
   :node     "data"
   :endpoint +markets/margin-interest+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("ShrtVol" . short-margin-trade-volume) ("LongVol" . long-margin-trade-volume)
              ("ShrtNegVol" . short-negotiable-margin-trade-volume) ("LongNegVol" . long-negotiable-margin-trade-volume)
              ("ShrtStdVol" . short-standardized-margin-trade-volume) ("LongStdVol" . long-standardized-margin-trade-volume)
              ("IssType" . issue-type))))

(defun get-margin-alert (&key code date from to)
  "Daily margin trading outstandings for issues subject to daily publication (TSE / JSF alert list) from JPX /markets/margin-alert. The PubReason field is exposed as a nested margin-alert-publication-reason instance."
  (create-jquants-instance
   :class    margin-alert
   :node     "data"
   :endpoint +markets/margin-alert+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("PubDate"       . published-date)
              ("AppDate"       . application-date)
              ("PubReason"     . publication-reason)
              ("ShrtOut"       . short-margin-outstanding)
              ("ShrtOutChg"    . short-margin-outstanding-change)
              ("ShrtOutRatio"  . short-margin-outstanding-ratio)
              ("LongOut"       . long-margin-outstanding)
              ("LongOutChg"    . long-margin-outstanding-change)
              ("LongOutRatio"  . long-margin-outstanding-ratio)
              ("SLRatio"       . short-long-ratio)
              ("ShrtNegOut"    . short-negotiable-margin-outstanding)
              ("ShrtNegOutChg" . short-negotiable-margin-outstanding-change)
              ("ShrtStdOut"    . short-standardized-margin-outstanding)
              ("ShrtStdOutChg" . short-standardized-margin-outstanding-change)
              ("LongNegOut"    . long-negotiable-margin-outstanding)
              ("LongNegOutChg" . long-negotiable-margin-outstanding-change)
              ("LongStdOut"    . long-standardized-margin-outstanding)
              ("LongStdOutChg" . long-standardized-margin-outstanding-change)
              ("TSEMrgnRegCls" . tse-margin-regulation-classification))))

(defun get-short-sale-value-and-ratio-by-sector (&key sector33 date from to)
  "Daily short-selling turnover values aggregated by 33-industry sector (with / without price restrictions, and excluding-short turnover) from JPX /markets/short-ratio."
  (create-jquants-instance
   :class    short-sale-value-and-ratio-by-sector
   :node     "data"
   :endpoint +markets/short-ratio+
   ;; Wire param is `s33' per the JPX spec; the &key arg name stays
   ;; `sector33' for back-compat.
   :params   `((s33 . ,sector33) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("S33" . sector33-code)
              ("SellExShortVa" . selling-excluding-short-selling-turnover-value)
              ("ShrtWithResVa" . short-selling-with-restrictions-turnover-value)
              ("ShrtNoResVa" . short-selling-without-restrictions-turnover-value))))

(defun get-short-sale-report (&key code disclosed-date calculated-date from to)
  "Outstanding short selling positions reports (mandatory disclosures of short positions ≥ 0.5% of issued shares) from JPX /markets/short-sale-report. FROM/TO bound the DisclosedDate range."
  (create-jquants-instance
   :class    short-sale-report
   :node     "data"
   :endpoint +markets/short-sale-report+
   ;; Wire-level param names match the JPX spec: disc_date / calc_date are
   ;; the per-endpoint date filters, and from/to map onto disc_date_from /
   ;; disc_date_to (the spec only exposes a range filter on DisclosedDate).
   ;; Lisp keyword args stay unchanged for back-compat.
   :params   `((code . ,code)
               (disc_date      . ,disclosed-date)
               (calc_date      . ,calculated-date)
               (disc_date_from . ,from)
               (disc_date_to   . ,to))
   ;; Response keys come back abbreviated (DiscDate / CalcDate, not the
   ;; spec page's CamelCase). Slot names disc-date / calc-date unchanged.
   :key-map '(("DiscDate"       . disc-date)
              ("CalcDate"       . calc-date)
              ("SSName"         . short-seller-name)
              ("SSAddr"         . short-seller-address)
              ("DICName"        . discretionary-investment-contractor-name)
              ("DICAddr"        . discretionary-investment-contractor-address)
              ("FundName"       . fund-name)
              ("ShrtPosToSO"    . short-positions-to-shares-outstanding-ratio)
              ("ShrtPosShares"  . short-positions-in-shares-number)
              ("ShrtPosUnits"   . short-positions-in-units-number)
              ("PrevRptRatio"   . short-positions-in-previous-reporting-ratio)
              ("PrevRptDate"    . short-positions-in-previous-reporting-date)
              ("Notes"          . notes))))

(defun get-trading-calendar (&key holidaydivision from to)
  "TSE / OSE business-day calendar with holiday classifications (and OSE holiday-trading flags) from JPX /markets/calendar."
  (create-jquants-instance
   :class    trading-calendar
   :node     "data"
   :endpoint +markets/calendar+
   ;; Wire param is `hol_div' per the JPX spec; the &key arg name stays
   ;; `holidaydivision' for back-compat.
   :params   `((hol_div . ,holidaydivision) (from . ,from) (to . ,to))
   :key-map '(("HolDiv" . holiday-division))))

(defun get-earnings-calendar ()
  "Upcoming earnings announcement schedule for companies with fiscal-year-end in March or September (JPX /equities/earnings-calendar)."
  (create-jquants-instance
   :class    earnings-calendar
   :node     "data"
   :endpoint +equities/earnings-calendar+
   :key-map '(("CoName" . company-name) ("FY" . fiscal-year) ("SectorNm" . sector-name)
              ("FQ" . fiscal-quarter) ("Section" . section))))

(defun get-financial-data (&key code date)
  "Quarterly earnings summary + dividend forecast revisions (Shikiho-style numeric data, JGAAP/IFRS) from JPX /fins/summary."
  (create-jquants-instance
   :class    financial-data
   :node     "data"
   :endpoint +fins/summary+
   :params   `((code . ,code) (date . ,date))
   :key-map '(("Code" . local-code)
              ("DiscDate" . disclosed-date) ("DiscTime" . disclosed-time) ("DiscNo" . disclosure-number)
              ("DocType" . type-of-document) ("CurPerType" . type-of-current-period)
              ("CurPerSt" . current-period-start-date) ("CurPerEn" . current-period-end-date)
              ("CurFYSt" . current-fiscal-year-start-date) ("CurFYEn" . current-fiscal-year-end-date)
              ("NxtFYSt" . next-fiscal-year-start-date) ("NxtFYEn" . next-fiscal-year-end-date)
              ("Sales" . net-sales) ("OP" . operating-profit) ("OdP" . ordinary-profit) ("NP" . profit)
              ("EPS" . earnings-per-share) ("DEPS" . diluted-earnings-per-share)
              ("TA" . total-assets) ("Eq" . equity) ("EqAR" . equity-to-asset-ratio) ("BPS" . book-value-per-share)
              ("CFO" . cash-flows-from-operating-activities) ("CFI" . cash-flows-from-investing-activities)
              ("CFF" . cash-flows-from-financing-activities) ("CashEq" . cash-and-equivalents)
              ("Div1Q" . result-dividend-per-share1st-quarter) ("Div2Q" . result-dividend-per-share2nd-quarter)
              ("Div3Q" . result-dividend-per-share3rd-quarter) ("DivFY" . result-dividend-per-share-fiscal-year-end)
              ("DivAnn" . result-dividend-per-share-annual) ("DivUnit" . distributions-per-unit-reit)
              ("DivTotalAnn" . result-total-dividend-paid-annual) ("PayoutRatioAnn" . result-payout-ratio-annual)
              ("FDiv1Q" . forecast-dividend-per-share1st-quarter) ("FDiv2Q" . forecast-dividend-per-share2nd-quarter)
              ("FDiv3Q" . forecast-dividend-per-share3rd-quarter) ("FDivFY" . forecast-dividend-per-share-fiscal-year-end)
              ("FDivAnn" . forecast-dividend-per-share-annual) ("FDivUnit" . forecast-distributions-per-unit-reit)
              ("FDivTotalAnn" . forecast-total-dividend-paid-annual) ("FPayoutRatioAnn" . forecast-payout-ratio-annual)
              ("NxFDiv1Q" . next-year-forecast-dividend-per-share1st-quarter) ("NxFDiv2Q" . next-year-forecast-dividend-per-share2nd-quarter)
              ("NxFDiv3Q" . next-year-forecast-dividend-per-share3rd-quarter) ("NxFDivFY" . next-year-forecast-dividend-per-share-fiscal-year-end)
              ("NxFDivAnn" . next-year-forecast-dividend-per-share-annual) ("NxFDivUnit" . next-year-forecast-distributions-per-unit-reit)
              ("NxFPayoutRatioAnn" . next-year-forecast-payout-ratio-annual)
              ("FSales2Q" . forecast-net-sales2nd-quarter) ("FOP2Q" . forecast-operating-profit2nd-quarter)
              ("FOdP2Q" . forecast-ordinary-profit2nd-quarter) ("FNP2Q" . forecast-profit2nd-quarter)
              ("FEPS2Q" . forecast-earnings-per-share2nd-quarter)
              ("NxFSales2Q" . next-year-forecast-net-sales2nd-quarter) ("NxFOP2Q" . next-year-forecast-operating-profit2nd-quarter)
              ("NxFOdP2Q" . next-year-forecast-ordinary-profit2nd-quarter) ("NxFNp2Q" . next-year-forecast-profit2nd-quarter)
              ("NxFEPS2Q" . next-year-forecast-earnings-per-share2nd-quarter)
              ("FSales" . forecast-net-sales) ("FOP" . forecast-operating-profit) ("FOdP" . forecast-ordinary-profit)
              ("FNP" . forecast-profit) ("FEPS" . forecast-earnings-per-share)
              ("NxFSales" . next-year-forecast-net-sales) ("NxFOP" . next-year-forecast-operating-profit)
              ("NxFOdP" . next-year-forecast-ordinary-profit) ("NxFNp" . next-year-forecast-profit)
              ("NxFEPS" . next-year-forecast-earnings-per-share)
              ("MatChgSub" . material-changes-in-subsidiaries) ("SigChgInC" . significant-changes-in-the-scope-of-consolidation)
              ("ChgByASRev" . changes-based-on-revisions-of-accounting-standard)
              ("ChgNoASRev" . changes-other-than-ones-based-on-revisions-of-accounting-standard)
              ("ChgAcEst" . changes-in-accounting-estimates) ("RetroRst" . retrospective-restatement)
              ("ShOutFY" . number-of-issued-and-outstanding-shares-at-the-end-of-fiscal-year-including-treasury-stock)
              ("TrShFY" . number-of-treasury-stock-at-the-end-of-fiscal-year) ("AvgSh" . average-number-of-shares)
              ("NCSales" . non-consolidated-net-sales) ("NCOP" . non-consolidated-operating-profit)
              ("NCOdP" . non-consolidated-ordinary-profit) ("NCNP" . non-consolidated-profit)
              ("NCEPS" . non-consolidated-earnings-per-share) ("NCTA" . non-consolidated-total-assets)
              ("NCEq" . non-consolidated-equity) ("NCEqAR" . non-consolidated-equity-to-asset-ratio)
              ("NCBPS" . non-consolidated-book-value-per-share)
              ("FNCSales2Q" . forecast-non-consolidated-net-sales2nd-quarter) ("FNCOP2Q" . forecast-non-consolidated-operating-profit2nd-quarter)
              ("FNCOdP2Q" . forecast-non-consolidated-ordinary-profit2nd-quarter) ("FNCNP2Q" . forecast-non-consolidated-profit2nd-quarter)
              ("FNCEPS2Q" . forecast-non-consolidated-earnings-per-share2nd-quarter)
              ("NxFNCSales2Q" . next-year-forecast-non-consolidated-net-sales2nd-quarter) ("NxFNCOP2Q" . next-year-forecast-non-consolidated-operating-profit2nd-quarter)
              ("NxFNCOdP2Q" . next-year-forecast-non-consolidated-ordinary-profit2nd-quarter) ("NxFNCNP2Q" . next-year-forecast-non-consolidated-profit2nd-quarter)
              ("NxFNCEPS2Q" . next-year-forecast-non-consolidated-earnings-per-share2nd-quarter)
              ("FNCSales" . forecast-non-consolidated-net-sales) ("FNCOP" . forecast-non-consolidated-operating-profit)
              ("FNCOdP" . forecast-non-consolidated-ordinary-profit) ("FNCNP" . forecast-non-consolidated-profit)
              ("FNCEPS" . forecast-non-consolidated-earnings-per-share)
              ("NxFNCSales" . next-year-forecast-non-consolidated-net-sales) ("NxFNCOP" . next-year-forecast-non-consolidated-operating-profit)
              ("NxFNCOdP" . next-year-forecast-non-consolidated-ordinary-profit) ("NxFNCNP" . next-year-forecast-non-consolidated-profit)
              ("NxFNCEPS" . next-year-forecast-non-consolidated-earnings-per-share))))

(defun get-breakdown-trading-data (&key code date from to)
  "Daily breakdown of trading value/volume by long/short and margin-new/margin-close from JPX /markets/breakdown."
  (create-jquants-instance
   :class    breakdown-trading-data
   :node     "data"
   :endpoint +markets/breakdown+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("LongSellVa" . long-sell-value) ("ShrtNoMrgnVa" . short-sell-without-margin-value)
              ("MrgnSellNewVa" . margin-sell-new-value) ("MrgnSellCloseVa" . margin-sell-close-value)
              ("LongBuyVa" . long-buy-value) ("MrgnBuyNewVa" . margin-buy-new-value) ("MrgnBuyCloseVa" . margin-buy-close-value)
              ("LongSellVo" . long-sell-volume) ("ShrtNoMrgnVo" . short-sell-without-margin-volume)
              ("MrgnSellNewVo" . margin-sell-new-volume) ("MrgnSellCloseVo" . margin-sell-close-volume)
              ("LongBuyVo" . long-buy-volume) ("MrgnBuyNewVo" . margin-buy-new-volume) ("MrgnBuyCloseVo" . margin-buy-close-volume))))

(defun get-morning-session-stock-prices (&key code)
  "Morning-session-only OHLCV per issue, published mid-day, from JPX /equities/bars/daily/am."
  (create-jquants-instance
   :class    morning-session-stock-prices
   :node     "data"
   :endpoint +equities/bars/daily/am+
   :params   `((code . ,code))
   :key-map '(("MO" . morning-open) ("MH" . morning-high) ("ML" . morning-low) ("MC" . morning-close)
              ("MVo" . morning-volume) ("MVa" . morning-turnover-value))))

(defun get-cash-dividend-data (&key code date from to)
  "Cash dividend forecasts and results — record / ex-rights / payment dates and per-share amounts — from JPX /fins/dividend."
  (create-jquants-instance
   :class    cash-dividend-data
   :node     "data"
   :endpoint +fins/dividend+
   :params   `((code . ,code) (date . ,date) (from . ,from) (to . ,to))
   :key-map '(("PubDate" . announcement-date) ("PubTime" . announcement-time)
              ("RefNo" . reference-number) ("StatCode" . status-code)
              ("BoardDate" . board-meeting-date) ("IFCode" . interim-final-code)
              ("FRCode" . forecast-result-code) ("IFTerm" . interim-final-term)
              ("DivRate" . gross-dividend-rate) ("RecDate" . record-date) ("ExDate" . ex-date)
              ("ActRecDate" . actual-record-date) ("PayDate" . payable-date)
              ("CARefNo" . ca-reference-number) ("DistAmt" . distribution-amount)
              ("RetEarn" . retained-earnings) ("DeemDiv" . deemed-dividend)
              ("DeemCapGains" . deemed-capital-gains) ("NetAssetDecRatio" . net-asset-decrease-ratio)
              ("CommSpecCode" . commemorative-special-code) ("CommDivRate" . commemorative-dividend-rate)
              ("SpecDivRate" . special-dividend-rate))))

(defun get-financial-statement-data (&key code date)
  "Full quarterly financial statements (BS / PL / CF) with EDINET XBRL verbose labels as keys, from JPX /fins/details."
  (create-jquants-instance
   :class    financial-statement-data
   :node     "data"
   :endpoint +fins/details+
   :params   `((code . ,code) (date . ,date))
   :key-map '(("Code" . local-code)
              ("DiscDate" . disclosed-date) ("DiscTime" . disclosed-time)
              ("DiscNo" . disclosure-number) ("DocType" . type-of-document) ("FS" . financial-statement))))

(defun get-futures-data (&key category date contract-flag)
  "Daily futures OHLC + settlement / open-interest / contract month (TOPIXF, NK225F, JGBLF, JN400F, …) from JPX /derivatives/bars/daily/futures."
  (create-jquants-instance
   :class    futures-data
   :node     "data"
   :endpoint +derivatives/bars/daily/futures+
   ;; Wire param is `contract_flag' per the JPX spec; &key arg unchanged.
   :params   `((category . ,category) (date . ,date) (contract_flag . ,contract-flag))
   :key-map '(("ProdCat" . derivatives-product-category)
              ("O" . whole-day-open) ("H" . whole-day-high) ("L" . whole-day-low) ("C" . whole-day-close)
              ("MO" . morning-session-open) ("MH" . morning-session-high) ("ML" . morning-session-low) ("MC" . morning-session-close)
              ("EO" . night-session-open) ("EH" . night-session-high) ("EL" . night-session-low) ("EC" . night-session-close)
              ("AO" . day-session-open) ("AH" . day-session-high) ("AL" . day-session-low) ("AC" . day-session-close)
              ("Vo" . volume) ("OI" . open-interest) ("Va" . turnover-value) ("CM" . contract-month)
              ("VoOA" . volume-only-auction) ("EmMrgnTrgDiv" . emergency-margin-trigger-division)
              ("LTD" . last-trading-day) ("SQD" . special-quotation-day) ("Settle" . settlement-price)
              ("CCMFlag" . central-contract-month-flag))))

(defun get-options-data (&key category date contract-flag)
  "Daily options OHLC + settlement / theoretical / IV / underlying-price for TOPIX, NK225, JGB futures options, and securities options, from JPX /derivatives/bars/daily/options."
  (create-jquants-instance
   :class    options-data
   :node     "data"
   :endpoint +derivatives/bars/daily/options+
   ;; Wire param is `contract_flag' per the JPX spec; &key arg unchanged.
   :params   `((category . ,category) (date . ,date) (contract_flag . ,contract-flag))
   :key-map '(("ProdCat" . derivatives-product-category) ("UndSSO" . underlying-sso)
              ("O" . whole-day-open) ("H" . whole-day-high) ("L" . whole-day-low) ("C" . whole-day-close)
              ("MO" . morning-session-open) ("MH" . morning-session-high) ("ML" . morning-session-low) ("MC" . morning-session-close)
              ("EO" . night-session-open) ("EH" . night-session-high) ("EL" . night-session-low) ("EC" . night-session-close)
              ("AO" . day-session-open) ("AH" . day-session-high) ("AL" . day-session-low) ("AC" . day-session-close)
              ("Vo" . volume) ("OI" . open-interest) ("Va" . turnover-value) ("CM" . contract-month)
              ("Strike" . strike-price) ("VoOA" . volume-only-auction) ("EmMrgnTrgDiv" . emergency-margin-trigger-division)
              ("PCDiv" . put-call-division) ("LTD" . last-trading-day) ("SQD" . special-quotation-day)
              ("Settle" . settlement-price) ("Theo" . theoretical-price) ("BaseVol" . base-volatility)
              ("UnderPx" . underlying-price) ("IV" . implied-volatility) ("IR" . interest-rate)
              ("CCMFlag" . central-contract-month-flag))))
