# Listed Issue Master (/equities/master)

`GET`    /v2/equities/master

## Overview

Listed issue information as of the past, the current day, and the next business day can be retrieved.\
Please note that listed issue information as of the next business day can be obtained after 17:30.

### Attention

> **Info**
>
> - For the specification of past dates, even if you are subscribing Premium plan and specify a date earlier than the start date of data provision (May 7, 2008), the issue information as of May 7, 2008 will be returned.
> - If specified "Date" is non-business day, the issue information as of next business day of specified date will be returned.

> **Note**
>
> In accordance with the TSE market restructuring in April 2022, the Bank of Japan (code: 83010) and Shinkin Central Bank (code: 84210) no longer belong to any market divisions under the system, but J-Quants handles them as "Standard".

## Obtain daily listed issue information

`GET` `https://api.jquants.com/v2/equities/master`

When acquiring data, issue code (code) or date (date) can be specified.\
The combination of each parameter and the results of the response are as below.

- code: –, date: – → All listed issues as of the day when API is executed. (\*1)

- code: ✓, date: – → Specified listed issues as of the day when API is executed. (\*1)

- code: –, date: ✓ → All listed issues as of the specified day. (\*2)

- code: ✓, date: ✓ → Specified listed issues as of the specified day. (\*2)

\*1 If "Date" is not specified on non-business day, the issue information as of next business day will be returned.\
\*2 If you are subscribing a plan other than free plan, data as of the next business day can be obtained. Even if you specify a future date that is earlier than the next business day, the issue information as of the next business day will be returned.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter | Type   | Required | Description                                                                                                                                                                                  |
| --------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code      | string | Optional | Issue code (e.g. 27890 or 2789) If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| date      | string | Optional | Date of application of information (e.g. 20210907 or 2021-09-07)                                                                                                                             |

### Sample Code

/v2/equities/master

**cURL**

```bash
curl -G https://api.jquants.com/v2/equities/master \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
  baseURL: 'https://api.jquants.com',
  headers: { 'x-api-key': '{{apiKey}}' },
})

await client.get('/v2/equities/master', {
  params: {
    code: '{{code}}',
    date: '{{date}}',
  },
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
    "https://api.jquants.com/v2/equities/master",
    params={"code": "{{code}}", "date": "{{date}}"},
    headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                                                                                                  |
| --------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------ |
| Date      | string | Required | Date of application of information (YYYY-MM-DD)                                                              |
| Code      | string | Required | Issue code                                                                                                   |
| CoName    | string | Required | Company Name (Japanese)                                                                                      |
| CoNameEn  | string | Required | Company Name (English)                                                                                       |
| S17       | string | Required | 17-Sector code (See [17-sector code and name](/en/spec/eq-master/sector17code))                              |
| S17Nm     | string | Required | 17-Sector code name (Japanese) (See [17-sector code and name](/en/spec/eq-master/sector17code))              |
| S33       | string | Required | 33-Sector code (See [33-sector code and name](/en/spec/eq-master/sector33code))                              |
| S33Nm     | string | Required | 33-Sector code name (Japanese) (See [33-sector code and name](/en/spec/eq-master/sector33code))              |
| ScaleCat  | string | Required | TOPIX Scale category                                                                                         |
| Mkt       | string | Required | Market segment code (See [Market segment code and name](/en/spec/eq-master/marketcode))                      |
| MktNm     | string | Required | Market segment code name (Japanese) (See [Market segment code and name](/en/spec/eq-master/marketcode))      |
| Mrgn      | string | Required | Flags of margin and loan issues (1: Margin issues / 2: Loan issues / 3: Other issues (non-loan, non-margin)) |
| MrgnNm    | string | Required | Name of flags of margin and loan issues                                                                      |
| ProdCat   | string | Required | Product category code (See [Product category codes and names](/en/spec/eq-master/product-category))          |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2022-11-11",
            "Code": "86970",
            "CoName": "日本取引所グループ",
            "CoNameEn": "Japan Exchange Group,Inc.",
            "S17": "16",
            "S17Nm": "金融（除く銀行）",
            "S33": "7200",
            "S33Nm": "その他金融業",
            "ScaleCat": "TOPIX Large70",
            "Mkt": "0111",
            "MktNm": "プライム",
            "Mrgn": "1",
            "MrgnNm": "信用",
            "ProdCat": "011"
        }
    ]
}
```

# Stock Prices (OHLC) (/equities/bars/daily)

`GET`    /v2/equities/bars/daily

## Overview

You can get information about stock price.\
Stock price consists before and after adjustment of stock splits and reverse stock splits (Rounded to first decimal places).

### Attention

> **Info**
>
> - Open, High, Low, Close, the volume of trade and the amount of purchase for the issue on the day when there is no trade volume (no sale) are recorded as Null.
> - Stocks that are not listed on the TSE (including issue listed only on the other exchanges) are not included in the data.
> - The data for Oct. 1st, 2020 are the OHLC, trading volume, and trading value in Null because trading was halted all day due to the failure of the equity trading system, arrowhead.
> - Daily prices can be obtained for all plans, but morning/afternoon session prices are available only for Premium plan.
> - Stock price adjustments are supported only for stock splits and reverse stock splits. Please note that some corporate actions are not supported.

## Get daily stock prices

`GET` `https://api.jquants.com/v2/equities/bars/daily`

In your request message, either "code" or "date" must be specified.

### Parameter and Response

In your request message, either "code" or "date" must be specified.\
Combination of parameter in the request and results are as below.

- code: ✓, date: –, from /to: – → All historical stock prices of a specific issue.

- code: ✓, date: ✓, from /to: – → Stock prices of a specific issue for the specific date

- code: ✓, date: –, from /to: ✓ → Stock prices of a specific issue for the specified period

- code: –, date: ✓, from /to: – → All listed issue prices for the specific date.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                                                                  |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 27800 or 2780) If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| date            | string | Optional | Date of data when "from" and "to" are not specified (e.g. 20210907 or 2021-09-07)                                                                                                            |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                                                                  |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                                                                       |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation.                                          |

### Sample Code

/v2/equities/bars/daily

**cURL**

```bash
curl -G https://api.jquants.com/v2/equities/bars/daily \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
  baseURL: 'https://api.jquants.com',
  headers: { 'x-api-key': '{{apiKey}}' },
})

await client.get('/v2/equities/bars/daily', {
  params: {
    code: '{{code}}',
    date: '{{date}}',
  },
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
    "https://api.jquants.com/v2/equities/bars/daily",
    params={"code": "{{code}}", "date": "{{date}}"},
    headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                                                                                                                                                 |
| --------- | ------ | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Date      | string | Required | Date (YYYY-MM-DD)                                                                                                                                           |
| Code      | string | Required | Issue code                                                                                                                                                  |
| O         | number | Required | Open Price (before adjustment)                                                                                                                              |
| H         | number | Required | High price (before adjustment)                                                                                                                              |
| L         | number | Required | Low price (before adjustment)                                                                                                                               |
| C         | number | Required | Close price (before adjustment)                                                                                                                             |
| UL        | string | Required | Flag of hitting the upper price limit of the day (0: Other than hitting the upper price limit, 1: Hitting the upper price limit)                            |
| LL        | string | Required | Flag of hitting the lower price limit of the day (0: Other than hitting the lower price limit, 1: Hitting the lower price limit)                            |
| Vo        | number | Required | Trading volume (before adjustment)                                                                                                                          |
| Va        | number | Required | Trading value                                                                                                                                               |
| AdjFactor | number | Required | Adjustment factor (In the case of a two-for-one stock split, "0.5" will be set in the record on the ex-rights date.)                                        |
| AdjO      | number | Required | Adjusted open price (\*1)                                                                                                                                   |
| AdjH      | number | Required | Adjusted high price (\*1)                                                                                                                                   |
| AdjL      | number | Required | Adjusted low price (\*1)                                                                                                                                    |
| AdjC      | number | Required | Adjusted close price (\*1)                                                                                                                                  |
| AdjVo     | number | Required | Adjusted volume (\*1)                                                                                                                                       |
| MO        | number | Required | Open price of the morning session (before adjustment) (\*2)                                                                                                 |
| MH        | number | Required | High price of the morning session (before adjustment) (\*2)                                                                                                 |
| ML        | number | Required | Low price of the morning session (before adjustment) (\*2)                                                                                                  |
| MC        | number | Required | Close price of the morning session (before adjustment) (\*2)                                                                                                |
| MUL       | string | Required | Flag of hitting the upper price limit of the day in morning session (0: Other than hitting the upper price limit, 1: Hitting the upper price limit) (\*2)   |
| MLL       | string | Required | Flag of hitting the lower price limit of the day in morning session (0: Other than hitting the lower price limit, 1: Hitting the lower price limit) (\*2)   |
| MVo       | number | Required | Trading volume of the morning session (before adjustment) (\*2)                                                                                             |
| MVa       | number | Required | Trading value of the morning session (\*2)                                                                                                                  |
| MAdjO     | number | Required | Adjusted open price of the morning session (\*1, \*2)                                                                                                       |
| MAdjH     | number | Required | Adjusted high price of the morning session (\*1, \*2)                                                                                                       |
| MAdjL     | number | Required | Adjusted low price of the morning session (\*1, \*2)                                                                                                        |
| MAdjC     | number | Required | Adjusted close price of the morning session (\*1, \*2)                                                                                                      |
| MAdjVo    | number | Required | Adjusted trading volume of the morning session (\*1, \*2)                                                                                                   |
| AO        | number | Required | Open price of the afternoon session (before adjustment) (\*2)                                                                                               |
| AH        | number | Required | High price of the afternoon session (before adjustment) (\*2)                                                                                               |
| AL        | number | Required | Low price of the afternoon session (before adjustment) (\*2)                                                                                                |
| AC        | number | Required | Close price of the afternoon session (before adjustment) (\*2)                                                                                              |
| AUL       | string | Required | Flag of hitting the upper price limit of the day in afternoon session (0: Other than hitting the upper price limit, 1: Hitting the upper price limit) (\*2) |
| ALL       | string | Required | Flag of hitting the lower price limit of the day in afternoon session (0: Other than hitting the lower price limit, 1: Hitting the lower price limit) (\*2) |
| AVo       | number | Required | Trading volume of the afternoon session (before adjustment) (\*2)                                                                                           |
| AVa       | number | Required | Trading value of the afternoon session (\*2)                                                                                                                |
| AAdjO     | number | Required | Adjusted open price of the afternoon session (\*1, \*2)                                                                                                     |
| AAdjH     | number | Required | Adjusted high price of the afternoon session (\*1, \*2)                                                                                                     |
| AAdjL     | number | Required | Adjusted low price of the afternoon session (\*1, \*2)                                                                                                      |
| AAdjC     | number | Required | Adjusted close price of the afternoon session (\*1, \*2)                                                                                                    |
| AAdjVo    | number | Required | Adjusted trading volume of the afternoon session (\*1, \*2)                                                                                                 |

\*1 The item has been adjusted to take into account past divisions, etc.\
\*2 The item is available only for Premium plan users.

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2023-03-24",
            "Code": "86970",
            "O": 2047.0,
            "H": 2069.0,
            "L": 2035.0,
            "C": 2045.0,
            "UL": "0",
            "LL": "0",
            "Vo": 2202500.0,
            "Va": 4507051850.0,
            "AdjFactor": 1.0,
            "AdjO": 2047.0,
            "AdjH": 2069.0,
            "AdjL": 2035.0,
            "AdjC": 2045.0,
            "AdjVo": 2202500.0,
            "MO": 2047.0,
            "MH": 2069.0,
            "ML": 2040.0,
            "MC": 2045.5,
            "MUL": "0",
            "MLL": "0",
            "MVo": 1121200.0,
            "MVa": 2297525850.0,
            "MAdjO": 2047.0,
            "MAdjH": 2069.0,
            "MAdjL": 2040.0,
            "MAdjC": 2045.5,
            "MAdjVo": 1121200.0,
            "AO": 2047.0,
            "AH": 2047.0,
            "AL": 2035.0,
            "AC": 2045.0,
            "AUL": "0",
            "ALL": "0",
            "AVo": 1081300.0,
            "AVa": 2209526000.0,
            "AAdjO": 2047.0,
            "AAdjH": 2047.0,
            "AAdjL": 2035.0,
            "AAdjC": 2045.0,
            "AAdjVo": 1081300.0
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Morning Session Stock Prices (OHLC) (/equities/bars/daily/am)

`GET`    /v2/equities/bars/daily/am

## Overview

You can obtain the morning session's high, low, opening, and closing prices for individual stocks as quick updates at noon.

### Attention

> **Info**
>
> - Null is recorded for the open, high, low, close, volume and trading value for stocks for which there is no trading volume in the morning session.
> - Stocks that are not listed on the TSE (including issue listed only on the other exchanges) are not included in the data.
> - Data for the day can be obtained until around 6:00 the next day.
>   For historical data, please use [Stock Prices (OHLC)](/en/spec/eq-bars-daily).

## Get stock prices in the morning session

`GET` `https://api.jquants.com/v2/equities/bars/daily/am`

In your request message, "code" can be specified.

### Parameter and Response

In your request message, "code" can be specified.\
Parameter in the request and results are as below.

- code: ✓ → A specified issue price in the morning session

- code: – → All listed issue prices in the morning session

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter       | Type   | Required | Description                                                                                                                                                                                       |
| --------------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 27800 or 2780)  If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation.                                          |

### Sample Code

/v2/equities/bars/daily/am

**cURL**

```bash
curl -G https://api.jquants.com/v2/equities/bars/daily/am \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: 'https://api.jquants.com',
headers: { 'x-api-key': '{{apiKey}}' },
})

await client.get('/v2/equities/bars/daily/am', {
params: {
  code: '{{code}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/equities/bars/daily/am",
  params={"code": "{{code}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                           |
| --------- | ------ | -------- | ------------------------------------- |
| Date      | string | Required | Date (YYYY-MM-DD)                     |
| Code      | string | Required | Issue code                            |
| MO        | number | Required | Open price of the morning session     |
| MH        | number | Required | High price of the morning session     |
| ML        | number | Required | Low price of the morning session      |
| MC        | number | Required | Close price of the morning session    |
| MVo       | number | Required | Trading volume of the morning session |
| MVa       | number | Required | Trading value of the morning session  |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2023-03-20",
            "Code": "39400",
            "MO": 232.0,
            "MH": 244.0,
            "ML": 232.0,
            "MC": 240.0,
            "MVo": 52600.0,
            "MVa": 12518800.0
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Trading by Type of Investors (/equities/investor-types)

`GET`    /v2/equities/investor-types

## Overview

Trading by type of investors (stock trading value) can be obtained.\
This data is also available via the following site. The data is set in units of 1000 yen.\
[https://www.jpx.co.jp/english/markets/statistics-equities/investor-type/index.html](https://www.jpx.co.jp/english/markets/statistics-equities/investor-type/index.html)

### Attention

> **Info**
>
> - In accordance with the market classification review conducted on April 4, 2022, statistical data that are based on market classifications have been changed to the new market segments.
> - When the data of trading by type of investors is revised, that is, value of the past data is modified, the data is provided by this API as follows.
>   - Revisions that are announced on or before April 3, 2023: only the data after revision is provided.
>   - Revisions that are announced on or after April 3, 2023: both the data before revision and after revision are provided. When a revision occurs, a record is added with the same Section, StartDate and EndDate. In such a case, data with the newer PublishedDate represents the revised data while the data with the older PublishedDate can be identified as the pre-correction data.
> - When the data of trading by type of investors is revised, the updated data will be available on the next business day after the correction is announced.

## Get trading by type of investors

`GET` `https://api.jquants.com/v2/equities/investor-types`

In your request message, either "section" or "from/to" can be specified.

### Parameter and Response

In your request message, either "section" or "from/to" can be specified.\
Combination of parameter in the request and results are as below.

- section: ✓, from /to: ✓ → Trading data of a specific section for the specified period.

- section: ✓, from /to: – → All trading data of a specific section.

- section: –, from /to: ✓ → Trading data of all sections for the specified period.

- section: –, from /to: – → All trading data of all sections for all available period.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| section         | string | Optional | Section name (e.g. TSEPrime) For a list of possible values, please see [here](/en/spec/eq-investor-types/section).                                       |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                              |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                                   |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/equities/investor-types

**cURL**

```bash
curl -G https://api.jquants.com/v2/equities/investor-types \
-H "x-api-key: {{apiKey}}" \
-d section="{{section}}" \
-d from="{{from}}" \
-d to="{{to}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: 'https://api.jquants.com',
headers: { 'x-api-key': '{{apiKey}}' },
})

await client.get('/v2/equities/investor-types', {
params: {
  section: '{{section}}',
  from: '{{from}}',
  to: '{{to}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/equities/investor-types",
  params={"section": "{{section}}", "from": "{{from}}", "to": "{{to}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter   | Type   | Required | Description                                                           |
| ----------- | ------ | -------- | --------------------------------------------------------------------- |
| PubDate     | string | Required | Published Date (YYYY-MM-DD)                                           |
| StDate      | string | Required | Start Date (YYYY-MM-DD)                                               |
| EnDate      | string | Required | End Date (YYYY-MM-DD)                                                 |
| Section     | string | Required | Section Name (See [Section name](/en/spec/eq-investor-types/section)) |
| PropSell    | number | Required | Proprietary Sales Value                                               |
| PropBuy     | number | Required | Proprietary Purchase Value                                            |
| PropTot     | number | Required | Proprietary Total Value                                               |
| PropBal     | number | Required | Proprietary Balance Value                                             |
| BrkSell     | number | Required | Brokerage Sales Value                                                 |
| BrkBuy      | number | Required | Brokerage Purchase Value                                              |
| BrkTot      | number | Required | Brokerage Total Value                                                 |
| BrkBal      | number | Required | Brokerage Balance Value                                               |
| TotSell     | number | Required | Total Sales Value                                                     |
| TotBuy      | number | Required | Total Purchase Value                                                  |
| TotTot      | number | Required | Total Value                                                           |
| TotBal      | number | Required | Total Balance Value                                                   |
| IndSell     | number | Required | Individuals Sales Value                                               |
| IndBuy      | number | Required | Individuals Purchase Value                                            |
| IndTot      | number | Required | Individuals Total Value                                               |
| IndBal      | number | Required | Individuals Balance Value                                             |
| FrgnSell    | number | Required | Foreigners Sales Value                                                |
| FrgnBuy     | number | Required | Foreigners Purchase Value                                             |
| FrgnTot     | number | Required | Foreigners Total Value                                                |
| FrgnBal     | number | Required | Foreigners Balance Value                                              |
| SecCoSell   | number | Required | Securities Companies Sales Value                                      |
| SecCoBuy    | number | Required | Securities Companies Purchase Value                                   |
| SecCoTot    | number | Required | Securities Companies Total Value                                      |
| SecCoBal    | number | Required | Securities Companies Balance Value                                    |
| InvTrSell   | number | Required | Investment Trusts Sales Value                                         |
| InvTrBuy    | number | Required | Investment Trusts Purchase Value                                      |
| InvTrTot    | number | Required | Investment Trusts Total Value                                         |
| InvTrBal    | number | Required | Investment Trusts Balance Value                                       |
| BusCoSell   | number | Required | Business Companies Sales Value                                        |
| BusCoBuy    | number | Required | Business Companies Purchase Value                                     |
| BusCoTot    | number | Required | Business Companies Total Value                                        |
| BusCoBal    | number | Required | Business Companies Balance Value                                      |
| OthCoSell   | number | Required | Other Companies Sales Value                                           |
| OthCoBuy    | number | Required | Other Companies Purchase Value                                        |
| OthCoTot    | number | Required | Other Companies Total Value                                           |
| OthCoBal    | number | Required | Other Companies Balance Value                                         |
| InsCoSell   | number | Required | Insurance Companies Sales Value                                       |
| InsCoBuy    | number | Required | Insurance Companies Purchase Value                                    |
| InsCoTot    | number | Required | Insurance Companies Total Value                                       |
| InsCoBal    | number | Required | Insurance Companies Balance Value                                     |
| BankSell    | number | Required | City Banks Regional Banks Etc Sales Value                             |
| BankBuy     | number | Required | City Banks Regional Banks Etc Purchase Value                          |
| BankTot     | number | Required | City Banks Regional Banks Etc Total Value                             |
| BankBal     | number | Required | City Banks Regional Banks Etc Balance Value                           |
| TrstBnkSell | number | Required | Trust Banks Sales Value                                               |
| TrstBnkBuy  | number | Required | Trust Banks Purchase Value                                            |
| TrstBnkTot  | number | Required | Trust Banks Total Value                                               |
| TrstBnkBal  | number | Required | Trust Banks Balance Value                                             |
| OthFinSell  | number | Required | Other Financial Institutions Sales Value                              |
| OthFinBuy   | number | Required | Other Financial Institutions Purchase Value                           |
| OthFinTot   | number | Required | Other Financial Institutions Total Value                              |
| OthFinBal   | number | Required | Other Financial Institutions Balance Value                            |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "PubDate": "2017-01-13",
            "StDate": "2017-01-04",
            "EnDate": "2017-01-06",
            "Section": "TSE1st",
            "PropSell": 1311271004,
            "PropBuy": 1453326508,
            "PropTot": 2764597512,
            "PropBal": 142055504,
            "BrkSell": 7165529005,
            "BrkBuy": 7030019854,
            "BrkTot": 14195548859,
            "BrkBal": -135509151,
            "TotSell": 8476800009,
            "TotBuy": 8483346362,
            "TotTot": 16960146371,
            "TotBal": 6546353,
            "IndSell": 1401711615,
            "IndBuy": 1161801155,
            "IndTot": 2563512770,
            "IndBal": -239910460,
            "FrgnSell": 5094891735,
            "FrgnBuy": 5317151774,
            "FrgnTot": 10412043509,
            "FrgnBal": 222260039,
            "SecCoSell": 76381455,
            "SecCoBuy": 61700100,
            "SecCoTot": 138081555,
            "SecCoBal": -14681355,
            "InvTrSell": 168705109,
            "InvTrBuy": 124389642,
            "InvTrTot": 293094751,
            "InvTrBal": -44315467,
            "BusCoSell": 71217959,
            "BusCoBuy": 63526641,
            "BusCoTot": 134744600,
            "BusCoBal": -7691318,
            "OthCoSell": 10745152,
            "OthCoBuy": 15687836,
            "OthCoTot": 26432988,
            "OthCoBal": 4942684,
            "InsCoSell": 15926202,
            "InsCoBuy": 9831555,
            "InsCoTot": 25757757,
            "InsCoBal": -6094647,
            "BankSell": 10606789,
            "BankBuy": 8843871,
            "BankTot": 19450660,
            "BankBal": -1762918,
            "TrstBnkSell": 292932297,
            "TrstBnkBuy": 245322795,
            "TrstBnkTot": 538255092,
            "TrstBnkBal": -47609502,
            "OthFinSell": 22410692,
            "OthFinBuy": 21764485,
            "OthFinTot": 44175177,
            "OthFinBal": -646207
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Margin Trading Outstandings (/markets/margin-interest)

`GET`    /v2/markets/margin-interest

## Overview

Weekly margin trading outstandings (number of shares) as of the last business day of each week is available.

This data is also available via the following site.\
[https://www.jpx.co.jp/english/markets/statistics-equities/margin/index.html](https://www.jpx.co.jp/english/markets/statistics-equities/margin/index.html)

### Attention

> **Info**
>
> - Even in the event of a corporate action for the issue, items of trading volume will not be retrospectively adjusted.
> - No data is provided for weeks with two or fewer business days like New Year's holiday.
> - Stocks that are not listed on the TSE (including issue listed only on the other exchanges) are not included in the data.

## Get weekly margin trading outstandings

`GET` `https://api.jquants.com/v2/markets/margin-interest`

Either "code" or "date" must be specified.

### Parameter and Response

Either "code" or "date" must be specified.\
Combination of parameter in the request and results are as below.

- code: ✓, date: –, from /to: – → All historical data of a specific issue.

- code: ✓, date: ✓, from /to: – → Data of a specific issue for the specific date.

- code: ✓, date: –, from /to: ✓ → Data of a specific issue for the specified period.

- code: –, date: ✓, from /to: – → All listed issue data for the specific date.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                                                                  |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 27800 or 2780) If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                                                                  |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                                                                       |
| date            | string | Optional | Date when "from" and "to" are not specified (e.g. 20210907 or 2021-09-07)                                                                                                                    |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation.                                          |

### Sample Code

/v2/markets/margin-interest

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/margin-interest \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/margin-interest", {
params: {
  code: '{{code}}',
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/markets/margin-interest",
  params={"code": "{{code}}", "date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter  | Type   | Required | Description                                                                                                                |
| ---------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------- |
| Date       | string | Required | Record date Base date of margin trading outstandings(usually on Friday) (YYYY-MM-DD)                                       |
| Code       | string | Required | Issue code                                                                                                                 |
| ShrtVol    | number | Required | Total margin trading weekend short positions                                                                               |
| LongVol    | number | Required | Total margin trading weekend long positions                                                                                |
| ShrtNegVol | number | Required | Negotiable margin trading weekend short positions Negotiable part of the total margin trading weekend short positions.     |
| LongNegVol | number | Required | Negotiable margin trading weekend long positions Negotiable part of the total margin trading weekend long positions.       |
| ShrtStdVol | number | Required | Standardized margin trading weekend short positions Standardized part of the total margin trading weekend short positions. |
| LongStdVol | number | Required | Standardized margin positions weekend long positions Standardized part of the total margin trading weekend long positions. |
| IssType    | string | Required | Issue Classifications 1: Margin issues, 2: Loan issues, 3: Other issues (non-loan, non-margin)                             |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2023-03-24",
            "Code": "86970",
            "ShrtVol": 123456.0,
            "LongVol": 234567.0,
            "ShrtNegVol": 11111.0,
            "LongNegVol": 22222.0,
            "ShrtStdVol": 33333.0,
            "LongStdVol": 44444.0,
            "IssType": "1"
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Short Sale Value and Ratio by Sector (/markets/short-ratio)

`GET`    /v2/markets/short-ratio

## Overview

You can obtain daily short sale trading values by industry (sector).\
This data is also available via the following site.\
[https://www.jpx.co.jp/english/markets/statistics-equities/short-selling/index.html](https://www.jpx.co.jp/english/markets/statistics-equities/short-selling/index.html) \
The published values on the web page are rounded to million yen, but this API provides data in yen.

### Attention

> **Info**
>
> - If a date is specified for which no trading volume exists (no sale), the value will be empty.
> - The data for Oct, 1st 2020 does not exist because trading was halted all day due to the failure of the equity trading system.

## Get daily short sale trading values by sector

`GET` `https://api.jquants.com/v2/markets/short-ratio`

When acquiring data, either "date" or "s33" (33-sector code) must be specified.

### Parameter and Response

When acquiring data, either "date" or "s33" (33-sector code) must be specified.\
The combination of each parameter and the results of the response are as below.

- s33: –, date: ✓, from/to: – → Short sale data of all sectors for the specified day.

- s33: ✓, date: –, from/to: – → Short sale data of the specified sector for all historical period.

- s33: ✓, date: –, from/to: ✓ → Short sale data of the specified sector for the specified period.

- s33: ✓, date: ✓, from/to: – → Short sale data of the specified sector for the specified day.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **s33** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                         |
| --------------- | ------ | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| s33             | string | Optional | 33-sector code (e.g. 0050 or 50)                                                                                                                    |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                         |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                              |
| date            | string | Optional | When "from" and "to" are not specified (e.g. 20210907 or 2021-09-07)                                                                                |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/markets/short-ratio

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/short-ratio \
-H "x-api-key: {{apiKey}}" \
-d s33="{{s33}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/short-ratio", {
params: {
  s33: '{{s33}}',
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/markets/short-ratio",
  params={"s33": "{{s33}}", "date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter     | Type   | Required | Description                                                                     |
| ------------- | ------ | -------- | ------------------------------------------------------------------------------- |
| Date          | string | Required | Date (YYYY-MM-DD)                                                               |
| S33           | string | Required | 33-sector code (See [33-sector code and name](/en/spec/eq-master/sector33code)) |
| SellExShortVa | number | Required | Trading value of long selling                                                   |
| ShrtWithResVa | number | Required | Value of short sales with price restrictions                                    |
| ShrtNoResVa   | number | Required | Value of short sales without price restrictions                                 |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2022-10-25",
            "S33": "0050",
            "SellExShortVa": 1333126400.0,
            "ShrtWithResVa": 787355200.0,
            "ShrtNoResVa": 149084300.0
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Outstanding Short Selling Positions Reported (/markets/short-sale-report)

`GET`    /v2/markets/short-sale-report

## Overview

This data covers the outstanding short selling position ratio is 0.5% or more of those reported by trading participants in accordance with the "Cabinet Office Order on Restrictions on Securities Transactions".

This data is the same as the following site but has a longer history.\
[https://www.jpx.co.jp/english/markets/public/short-selling/index.html](https://www.jpx.co.jp/english/markets/public/short-selling/index.html)

### Attention

> **Info**
>
> - Data will not be provided on days when no applicable reports are made by trading participants.
> - Please click here for the "Cabinet Office Order on Restrictions on Securities Transactions": [https://www.jpx.co.jp/english/markets/public/short-selling/01.html](https://www.jpx.co.jp/english/markets/public/short-selling/01.html)

## Get Outstanding Short Selling Positions Reported

`GET` `https://api.jquants.com/v2/markets/short-sale-report`

In your request message, either "code", "disc\_date" or "calc\_date" must be specified.

### Parameter and Response

In your request message, either "code", "disc\_date" or "calc\_date" must be specified.\
The combination of each parameter and the results of the response are as below.

- code: ✓, disc\_date: –, disc\_date\_from/disc\_date\_to: –, calc\_date: – → All historical data of a specific issue.

- code: ✓, disc\_date: ✓, disc\_date\_from/disc\_date\_to: –, calc\_date: – → Data of a specific issue for the specific date (DisclosedDate).

- code: ✓, disc\_date: –, disc\_date\_from/disc\_date\_to: ✓, calc\_date: – → Data of a specific issue for the specified period.

- code: ✓, disc\_date: –, disc\_date\_from/disc\_date\_to: –, calc\_date: ✓ → Data of a specific issue for the specific date (CalculatedDate).

- code: –, disc\_date: ✓, disc\_date\_from/disc\_date\_to: –, calc\_date: – → Data for all listed issues on the specified date (DisclosedDate).

- code: –, disc\_date: –, disc\_date\_from/disc\_date\_to: –, calc\_date: ✓ → Data for all listed issues on the specified date (CalculatedDate).

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> At least one of **code** / **disc\_date** / **calc\_date** must be specified.

| Parameter        | Type   | Required | Description                                                                                                                                                                                                        |
| ---------------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| code             | string | Optional | 4 or 5 character issue code (e.g. 8697 or 86970)  If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| disc\_date       | string | Optional | Date of Disclosure (e.g. 20240301 or 2024-03-01)                                                                                                                                                                   |
| disc\_date\_from | string | Optional | Starting point of data period (e.g. 20240301 or 2024-03-01)                                                                                                                                                        |
| disc\_date\_to   | string | Optional | End point of data period (e.g. 20240301 or 2024-03-01)                                                                                                                                                             |
| calc\_date       | string | Optional | Date of Calculation (e.g. 20240301 or 2024-03-01)                                                                                                                                                                  |
| pagination\_key  | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation.                                                                |

### Sample Code

/v2/markets/short-sale-report

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/short-sale-report \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d calc_date="{{calc_date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/short-sale-report", {
params: {
  code: '{{code}}',
  calc_date: '{{calc_date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/markets/short-sale-report",
  params={"code": "{{code}}", "calc_date": "{{calc_date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter     | Type   | Required | Description                                                                                                                                                                          |
| ------------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| DiscDate      | string | Required | Date of Disclosure (YYYY-MM-DD)                                                                                                                                                      |
| CalcDate      | string | Required | Date of Calculation (YYYY-MM-DD)                                                                                                                                                     |
| Code          | string | Required | Issue code (5-character)                                                                                                                                                             |
| SSName        | string | Required | Name of Short Seller  The value is listed as reported by market participants, so both Japanese and English names are mixed. Please note that "個人" refers to individual investor. |
| SSAddr        | string | Required | Address of Short Seller                                                                                                                                                              |
| DICName       | string | Required | Name of Discretionary Investment Contractor                                                                                                                                          |
| DICAddr       | string | Required | Address of Discretionary Investment Contractor                                                                                                                                       |
| FundName      | string | Required | Name of Investment Fund                                                                                                                                                              |
| ShrtPosToSO   | number | Required | Ratio of Short Positions to Shares Outstanding                                                                                                                                       |
| ShrtPosShares | number | Required | Number of Short Positions in Shares                                                                                                                                                  |
| ShrtPosUnits  | number | Required | Number of Short Positions in Trading Units                                                                                                                                           |
| PrevRptDate   | string | Required | Date of Calculation in Previous Reporting (YYYY-MM-DD)                                                                                                                               |
| PrevRptRatio  | number | Required | Ratio of Short Positions in Previous Reporting                                                                                                                                       |
| Notes         | string | Required | Notes                                                                                                                                                                                |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
      {
        "DiscDate": "2024-08-01",
        "CalcDate": "2024-07-31",
        "Code": "13660",
        "SSName": "個人",
        "SSAddr": "",
        "DICName": "",
        "DICAddr": "",
        "FundName": "",
        "ShrtPosToSO": 0.0053,
        "ShrtPosShares": 140000,
        "ShrtPosUnits": 140000,
        "PrevRptDate": "2024-07-22",
        "PrevRptRatio": 0.0043,
        "Notes": ""
      }
    ],
    "pagination_key": "value1.value2."
}
```

# Margin Trading Outstanding (Issues Subject to Daily Publication) (/markets/margin-alert)

`GET`    /v2/markets/margin-alert

## Overview

Daily margin trading outstanding as of the last business day is available.

This data is also available via the following site but no historical data.\
[https://www.jpx.co.jp/english/markets/statistics-equities/margin/index.html](https://www.jpx.co.jp/english/markets/statistics-equities/margin/index.html)

### Attention

> **Info**
>
> - No retroactive adjustment will be made to the data that have undergone corporate action.
> - Only stocks for which the Tokyo Stock Exchange or Japan Securities Finance Co., Ltd (JSF) decide it necessary to disclose Daily margin trading outstandings are included.
> - If the past data is revised, the data is provided by this API as follows;
>   - Both the data before revision and after revision are provided. When a revision occurs, a record with the same ApplicationDate is added. In such a case, data with the newer PublishedDate represents the revised data while the data with the older PublishedDate can be identified as the pre-correction data.

## Get daily margin trading outstandings

`GET` `https://api.jquants.com/v2/markets/margin-alert`

In your request message, either "code" or "date" must be specified.

### Parameter and Response

In your request message, either "code" or "date" must be specified.\
The combination of each parameter and the results of the response are as below.

- code: ✓, date: –, from /to: – → All historical data of a specific issue.

- code: ✓, date: ✓, from /to: – → Data of a specific issue for a specific published date.

- code: ✓, date: –, from /to: ✓ → Data of a specific issue for the specified period.

- code: –, date: ✓, from /to: – → All listed issue data for the specific date.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                                                                  |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 27800 or 2780) If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                                                                  |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                                                                       |
| date            | string | Optional | Date of data when from and to are not specified (e.g. 20210907 or 2021-09-07)                                                                                                                |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation.                                          |

### Sample Code

/v2/markets/margin-alert

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/margin-alert \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
  baseURL: "https://api.jquants.com",
  headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/margin-alert", {
  params: {
    code: '{{code}}',
    date: '{{date}}',
  },
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
    "https://api.jquants.com/v2/markets/margin-alert",
    params={"code": "{{code}}", "date": "{{date}}"},
    headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter     | Type            | Required | Description                                                                                       |
| ------------- | --------------- | -------- | ------------------------------------------------------------------------------------------------- |
| PubDate       | string          | Required | Published Date (YYYY-MM-DD)                                                                       |
| Code          | string          | Required | Issue code                                                                                        |
| AppDate       | string          | Required | Application Date (YYYY-MM-DD) The point in time when the margin trade volume                      |
| PubReason     | map             | Required | [Publish Reason](/en/spec/mkt-margin-alert/publish-reason)                                        |
| ShrtOut       | number          | Required | Total short positions (negotiable + standardized)                                                 |
| ShrtOutChg    | number / string | Required | Prev. day change in short positions (unit: share) If not published prev. day, set -.              |
| ShrtOutRatio  | number / string | Required | ShortMarginOutstanding / Listed shares × 100 (%) For ETF, set \*                                  |
| LongOut       | number          | Required | Total long positions (negotiable + standardized)                                                  |
| LongOutChg    | number / string | Required | Prev. day change in long positions (unit: share) If not published prev. day, set -.               |
| LongOutRatio  | number / string | Required | LongMarginOutstanding / Listed shares × 100 (%) For ETF, set \*                                   |
| SLRatio       | number          | Required | LongMarginOutstanding / ShortMarginOutstanding × 100 (%)                                          |
| ShrtNegOut    | number          | Required | Negotiable short positions Negotiable part of total short positions                               |
| ShrtNegOutChg | number / string | Required | Prev. day change in negotiable short positions (unit: share) If not published prev. day, set -.   |
| ShrtStdOut    | number          | Required | Standardized short positions Standardized part of total short positions                           |
| ShrtStdOutChg | number / string | Required | Prev. day change in standardized short positions (unit: share) If not published prev. day, set -. |
| LongNegOut    | number          | Required | Negotiable long positions Negotiable part of total long positions                                 |
| LongNegOutChg | number / string | Required | Prev. day change in negotiable long positions (unit: share) If not published prev. day, set -.    |
| LongStdOut    | number          | Required | Standardized long positions Standardized part of total long positions                             |
| LongStdOutChg | number / string | Required | Prev. day change in standardized long positions (unit: share) If not published prev. day, set -.  |
| TSEMrgnRegCls | string          | Required | [TSE Margin Regulation Classification](/en/spec/mkt-margin-alert/margin-trading-classification)   |

### Sample Response

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "PubDate": "2024-02-08",
            "Code": "13260",
            "AppDate": "2024-02-07",
            "PubReason":
                {
                    "Restricted": "0",
                    "DailyPublication": "0",
                    "Monitoring": "0",
                    "RestrictedByJSF": "0",
                    "PrecautionByJSF": "1",
                    "UnclearOrSecOnAlert": "0"
                },
            "ShrtOut": 11.0,
            "ShrtOutChg": 0.0,
            "ShrtOutRatio": "*",
            "LongOut": 676.0,
            "LongOutChg": -20.0,
            "LongOutRatio": "*",
            "SLRatio": 1.6,
            "ShrtNegOut": 0.0,
            "ShrtNegOutChg": 0.0,
            "ShrtStdOut": 11.0,
            "ShrtStdOutChg": 0.0,
            "LongNegOut": 192.0,
            "LongNegOutChg": -20.0,
            "LongStdOut": 484.0,
            "LongStdOutChg": 0.0,
            "TSEMrgnRegCls": "001"
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Margin Trading Outstanding (Issues Subject to Daily Publication) (/markets/margin-alert)

`GET`    /v2/markets/margin-alert

## Overview

Daily margin trading outstanding as of the last business day is available.

This data is also available via the following site but no historical data.\
[https://www.jpx.co.jp/english/markets/statistics-equities/margin/index.html](https://www.jpx.co.jp/english/markets/statistics-equities/margin/index.html)

### Attention

> **Info**
>
> - No retroactive adjustment will be made to the data that have undergone corporate action.
> - Only stocks for which the Tokyo Stock Exchange or Japan Securities Finance Co., Ltd (JSF) decide it necessary to disclose Daily margin trading outstandings are included.
> - If the past data is revised, the data is provided by this API as follows;
>   - Both the data before revision and after revision are provided. When a revision occurs, a record with the same ApplicationDate is added. In such a case, data with the newer PublishedDate represents the revised data while the data with the older PublishedDate can be identified as the pre-correction data.

## Get daily margin trading outstandings

`GET` `https://api.jquants.com/v2/markets/margin-alert`

In your request message, either "code" or "date" must be specified.

### Parameter and Response

In your request message, either "code" or "date" must be specified.\
The combination of each parameter and the results of the response are as below.

- code: ✓, date: –, from /to: – → All historical data of a specific issue.

- code: ✓, date: ✓, from /to: – → Data of a specific issue for a specific published date.

- code: ✓, date: –, from /to: ✓ → Data of a specific issue for the specified period.

- code: –, date: ✓, from /to: – → All listed issue data for the specific date.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                                                                  |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 27800 or 2780) If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                                                                  |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                                                                       |
| date            | string | Optional | Date of data when from and to are not specified (e.g. 20210907 or 2021-09-07)                                                                                                                |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation.                                          |

### Sample Code

/v2/markets/margin-alert

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/margin-alert \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
  baseURL: "https://api.jquants.com",
  headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/margin-alert", {
  params: {
    code: '{{code}}',
    date: '{{date}}',
  },
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
    "https://api.jquants.com/v2/markets/margin-alert",
    params={"code": "{{code}}", "date": "{{date}}"},
    headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter     | Type            | Required | Description                                                                                       |
| ------------- | --------------- | -------- | ------------------------------------------------------------------------------------------------- |
| PubDate       | string          | Required | Published Date (YYYY-MM-DD)                                                                       |
| Code          | string          | Required | Issue code                                                                                        |
| AppDate       | string          | Required | Application Date (YYYY-MM-DD) The point in time when the margin trade volume                      |
| PubReason     | map             | Required | [Publish Reason](/en/spec/mkt-margin-alert/publish-reason)                                        |
| ShrtOut       | number          | Required | Total short positions (negotiable + standardized)                                                 |
| ShrtOutChg    | number / string | Required | Prev. day change in short positions (unit: share) If not published prev. day, set -.              |
| ShrtOutRatio  | number / string | Required | ShortMarginOutstanding / Listed shares × 100 (%) For ETF, set \*                                  |
| LongOut       | number          | Required | Total long positions (negotiable + standardized)                                                  |
| LongOutChg    | number / string | Required | Prev. day change in long positions (unit: share) If not published prev. day, set -.               |
| LongOutRatio  | number / string | Required | LongMarginOutstanding / Listed shares × 100 (%) For ETF, set \*                                   |
| SLRatio       | number          | Required | LongMarginOutstanding / ShortMarginOutstanding × 100 (%)                                          |
| ShrtNegOut    | number          | Required | Negotiable short positions Negotiable part of total short positions                               |
| ShrtNegOutChg | number / string | Required | Prev. day change in negotiable short positions (unit: share) If not published prev. day, set -.   |
| ShrtStdOut    | number          | Required | Standardized short positions Standardized part of total short positions                           |
| ShrtStdOutChg | number / string | Required | Prev. day change in standardized short positions (unit: share) If not published prev. day, set -. |
| LongNegOut    | number          | Required | Negotiable long positions Negotiable part of total long positions                                 |
| LongNegOutChg | number / string | Required | Prev. day change in negotiable long positions (unit: share) If not published prev. day, set -.    |
| LongStdOut    | number          | Required | Standardized long positions Standardized part of total long positions                             |
| LongStdOutChg | number / string | Required | Prev. day change in standardized long positions (unit: share) If not published prev. day, set -.  |
| TSEMrgnRegCls | string          | Required | [TSE Margin Regulation Classification](/en/spec/mkt-margin-alert/margin-trading-classification)   |

### Sample Response

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "PubDate": "2024-02-08",
            "Code": "13260",
            "AppDate": "2024-02-07",
            "PubReason":
                {
                    "Restricted": "0",
                    "DailyPublication": "0",
                    "Monitoring": "0",
                    "RestrictedByJSF": "0",
                    "PrecautionByJSF": "1",
                    "UnclearOrSecOnAlert": "0"
                },
            "ShrtOut": 11.0,
            "ShrtOutChg": 0.0,
            "ShrtOutRatio": "*",
            "LongOut": 676.0,
            "LongOutChg": -20.0,
            "LongOutRatio": "*",
            "SLRatio": 1.6,
            "ShrtNegOut": 0.0,
            "ShrtNegOutChg": 0.0,
            "ShrtStdOut": 11.0,
            "ShrtStdOutChg": 0.0,
            "LongNegOut": 192.0,
            "LongNegOutChg": -20.0,
            "LongStdOut": 484.0,
            "LongStdOutChg": 0.0,
            "TSEMrgnRegCls": "001"
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Trading Calendar (/markets/calendar)

`GET`    /v2/markets/calendar

## Overview

Information on business days and non-business days at the Tokyo Stock Exchange (TSE) and Osaka Exchange (OSE), as well as whether holiday trading is conducted at OSE, can be obtained.\
The delivered data is the same as the content published on the following pages.

- Market Holidays: [https://www.jpx.co.jp/english/corporate/about-jpx/calendar/index.html](https://www.jpx.co.jp/english/corporate/about-jpx/calendar/index.html)
- Holiday Trading: [https://www.jpx.co.jp/english/derivatives/rules/holidaytrading/index.html](https://www.jpx.co.jp/english/derivatives/rules/holidaytrading/index.html)

### Attention

> **Info**
>
> - As a rule, the business days and holiday trading days (planned) for the following year will be updated around the end of March each year.

## Get business days data

`GET` `https://api.jquants.com/v2/markets/calendar`

You can specify a holiday division (hol\_div) or date period (from/to).

### Parameter and Response

You can specify a holiday division (hol\_div) or date period (from/to).\
The combination of each parameter and the results of the response are as below.

- hol\_div: ✓, from /to: – → All data for the specified holiday division.

- hol\_div: ✓, from /to: ✓ → Data for the specified holiday division for the specified period.

- hol\_div: –, from /to: ✓ → Data for the specified period.

- hol\_div: –, from /to: – → All data.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter | Type   | Required | Description                                                                                                |
| --------- | ------ | -------- | ---------------------------------------------------------------------------------------------------------- |
| hol\_div  | string | Optional | Holiday division  For a list of possible values, please see [here](/en/spec/mkt-cal/holiday-division). |
| from      | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                |
| to        | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                     |

### Sample Code

/v2/markets/calendar

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/calendar \
-H "x-api-key: {{apiKey}}" \
-d hol_div="{{hol_div}}" \
-d from="{{from}}" \
-d to="{{to}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/calendar", {
params: {
  hol_div: '{{hol_div}}',
  from: '{{from}}',
  to: '{{to}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/markets/calendar",
  params={
      "hol_div": "{{hol_div}}",
      "from": "{{from}}",
      "to": "{{to}}",
  },
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                                                                     |
| --------- | ------ | -------- | ------------------------------------------------------------------------------- |
| Date      | string | Required | Date (YYYY-MM-DD)                                                               |
| HolDiv    | string | Required | Holiday division  See [Holiday division](/en/spec/mkt-cal/holiday-division) |

### Sample Response

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2015-04-01",
            "HolDiv": "1"
        }
    ]
}
```

# Trading Calendar (/markets/calendar)

`GET`    /v2/markets/calendar

## Overview

Information on business days and non-business days at the Tokyo Stock Exchange (TSE) and Osaka Exchange (OSE), as well as whether holiday trading is conducted at OSE, can be obtained.\
The delivered data is the same as the content published on the following pages.

- Market Holidays: [https://www.jpx.co.jp/english/corporate/about-jpx/calendar/index.html](https://www.jpx.co.jp/english/corporate/about-jpx/calendar/index.html)
- Holiday Trading: [https://www.jpx.co.jp/english/derivatives/rules/holidaytrading/index.html](https://www.jpx.co.jp/english/derivatives/rules/holidaytrading/index.html)

### Attention

> **Info**
>
> - As a rule, the business days and holiday trading days (planned) for the following year will be updated around the end of March each year.

## Get business days data

`GET` `https://api.jquants.com/v2/markets/calendar`

You can specify a holiday division (hol\_div) or date period (from/to).

### Parameter and Response

You can specify a holiday division (hol\_div) or date period (from/to).\
The combination of each parameter and the results of the response are as below.

- hol\_div: ✓, from /to: – → All data for the specified holiday division.

- hol\_div: ✓, from /to: ✓ → Data for the specified holiday division for the specified period.

- hol\_div: –, from /to: ✓ → Data for the specified period.

- hol\_div: –, from /to: – → All data.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter | Type   | Required | Description                                                                                                |
| --------- | ------ | -------- | ---------------------------------------------------------------------------------------------------------- |
| hol\_div  | string | Optional | Holiday division  For a list of possible values, please see [here](/en/spec/mkt-cal/holiday-division). |
| from      | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                |
| to        | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                     |

### Sample Code

/v2/markets/calendar

**cURL**

```bash
curl -G https://api.jquants.com/v2/markets/calendar \
-H "x-api-key: {{apiKey}}" \
-d hol_div="{{hol_div}}" \
-d from="{{from}}" \
-d to="{{to}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/markets/calendar", {
params: {
  hol_div: '{{hol_div}}',
  from: '{{from}}',
  to: '{{to}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/markets/calendar",
  params={
      "hol_div": "{{hol_div}}",
      "from": "{{from}}",
      "to": "{{to}}",
  },
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                                                                     |
| --------- | ------ | -------- | ------------------------------------------------------------------------------- |
| Date      | string | Required | Date (YYYY-MM-DD)                                                               |
| HolDiv    | string | Required | Holiday division  See [Holiday division](/en/spec/mkt-cal/holiday-division) |

### Sample Response

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2015-04-01",
            "HolDiv": "1"
        }
    ]
}
```

# TOPIX Prices (OHLC) (/indices/bars/daily/topix)

`GET`    /v2/indices/bars/daily/topix

## Overview

Available index is TOPIX (Tokyo Stock Price Index).

## Get Daily TOPIX Information

`GET` `https://api.jquants.com/v2/indices/bars/daily/topix`

"from/to" can be specified (Optional). If "from/to" is not specified, the response contains all historical data.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter       | Type   | Required | Description                                                                                                                                         |
| --------------- | ------ | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------- |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                         |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                              |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate. Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/indices/bars/daily/topix

**cURL**

```bash
curl -G https://api.jquants.com/v2/indices/bars/daily/topix \
-H "x-api-key: {{apiKey}}" \
-d from="{{from}}" \
-d to="{{to}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/indices/bars/daily/topix", {
params: {
  from: '{{from}}',
  to: '{{to}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/indices/bars/daily/topix",
  params={
      "from": "{{from}}",
      "to": "{{to}}",
  },
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description       |
| --------- | ------ | -------- | ----------------- |
| Date      | string | Required | Date (YYYY-MM-DD) |
| O         | number | Required | Open Price        |
| H         | number | Required | High Price        |
| L         | number | Required | Low Price         |
| C         | number | Required | Close Price       |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2022-06-28",
            "O": 1885.52,
            "H": 1907.38,
            "L": 1885.32,
            "C": 1907.38
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Financial Data (Summary only) (/fins/summary)

`GET`    /v2/fins/summary

## Overview

You can obtain quarterly earnings summaries and disclosure information (mainly numerical data) on revisions to earnings and dividend information for listed companies.\
Either issue code (code) or date (date) must be specified.

## Attention

> **Info**
>
> - **About Accounting Standards:** Each item name output from the API is based on Japanese GAAP (JGAAP) disclosure items. Therefore, IFRS and U.S. GAAP (USGAAP) disclosure data do not have the concept of ordinary income, so the data is blank.

> **Info**
>
> - **About addition of API item in response to the "Revision of the Quarterly Disclosure System":**
>   - In response to the "Revision of the Quarterly Disclosure System", the items to be described in the Summary Form of Financial Statements will be changed as below.
>     - **before:** "Changes in significant subsidiaries during the period (changes in specified subsidiaries resulting in the change in scope of consolidation)"
>     - **after:** "Significant changes in the scope of consolidation during the period"
>   - In response to this change, "SignificantChangesInTheScopeOfConsolidation" is added to the response items of this API from Jul 22, 2024.
>   - For details, please refer to the Data Item column.

## Get quarterly financial information

`GET` `https://api.jquants.com/v2/fins/summary`

Either "code" or "date" must be specified.

### Parameter and Response

Either "code" or "date" must be specified.\
Combination of parameter in the request and results are as below.

- code: ✓, date: – → All financial data for a specific issue.

- code: ✓, date: ✓ → Financial data for a specific issue on the specific date.

- code: –, date: ✓ → Financial data for all listed issues on the specific date.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 86970 or 8697)  4 or 5 character issue code                                                                                         |
| date            | string | Optional | Disclosure date (e.g. 2022-01-05 or 20220105)                                                                                                            |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/fins/summary

**cURL**

```bash
curl -G https://api.jquants.com/v2/fins/summary \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: 'https://api.jquants.com',
headers: { 'x-api-key': '{{apiKey}}' },
})

await client.get('/v2/fins/summary', {
params: {
  code: '{{code}}',
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/fins/summary",
  params={"code": "{{code}}", "date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter         | Type   | Required | Description                                                                                                                                             |
| ----------------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DiscDate          | string | Required | Disclosed Date                                                                                                                                          |
| DiscTime          | string | Required | Disclosed Time                                                                                                                                          |
| Code              | string | Required | Issue code (5 digits)                                                                                                                                   |
| DiscNo            | string | Required | Disclosure Number  The json output from the API is sorted in ascending order by disclosure number.                                                  |
| DocType           | string | Required | Type of Document  [Type of Document List](/en/spec/fin-summary/typeofdocument)                                                                      |
| CurPerType        | string | Required | Type of Current Period  \[1Q, 2Q, 3Q, 4Q, 5Q, FY]                                                                                                   |
| CurPerSt          | string | Required | Current Period Start Date                                                                                                                               |
| CurPerEn          | string | Required | Current Period End Date                                                                                                                                 |
| CurFYSt           | string | Required | Current Fiscal Year Start Date                                                                                                                          |
| CurFYEn           | string | Required | Current Fiscal Year End Date                                                                                                                            |
| NxtFYSt           | string | Required | Next Fiscal Year Start Date  Empty if no next fiscal year disclosure information in the record.                                                     |
| NxtFYEn           | string | Required | Next Fiscal Year End Date  Empty if no next fiscal year disclosure information in the record.                                                       |
| Sales             | number | Required | Net Sales                                                                                                                                               |
| OP                | number | Required | Operating Profit                                                                                                                                        |
| OdP               | number | Required | Ordinary Profit                                                                                                                                         |
| NP                | number | Required | Profit                                                                                                                                                  |
| EPS               | number | Required | Earnings Per Share                                                                                                                                      |
| DEPS              | number | Required | Diluted Earnings Per Share                                                                                                                              |
| TA                | number | Required | Total Assets                                                                                                                                            |
| Eq                | number | Required | Equity                                                                                                                                                  |
| EqAR              | number | Required | Equity to Asset Ratio                                                                                                                                   |
| BPS               | number | Required | Book Value Per Share                                                                                                                                    |
| CFO               | number | Required | Cash Flows from Operating Activities                                                                                                                    |
| CFI               | number | Required | Cash Flows from Investing Activities                                                                                                                    |
| CFF               | number | Required | Cash Flows from Financing Activities                                                                                                                    |
| CashEq            | number | Required | Cash and Equivalents                                                                                                                                    |
| Div1Q             | number | Required | Result Dividend Per Share 1st Quarter                                                                                                                   |
| Div2Q             | number | Required | Result Dividend Per Share 2nd Quarter                                                                                                                   |
| Div3Q             | number | Required | Result Dividend Per Share 3rd Quarter                                                                                                                   |
| DivFY             | number | Required | Result Dividend Per Share Fiscal Year End                                                                                                               |
| DivAnn            | number | Required | Result Dividend Per Share Annual                                                                                                                        |
| DivUnit           | number | Required | Distributions Per Unit (REIT)                                                                                                                           |
| DivTotalAnn       | number | Required | Result Total Dividend Paid Annual                                                                                                                       |
| PayoutRatioAnn    | number | Required | Result Payout Ratio Annual                                                                                                                              |
| FDiv1Q            | number | Required | Forecast Dividend Per Share 1st Quarter                                                                                                                 |
| FDiv2Q            | number | Required | Forecast Dividend Per Share 2nd Quarter                                                                                                                 |
| FDiv3Q            | number | Required | Forecast Dividend Per Share 3rd Quarter                                                                                                                 |
| FDivFY            | number | Required | Forecast Dividend Per Share Fiscal Year End                                                                                                             |
| FDivAnn           | number | Required | Forecast Dividend Per Share Annual                                                                                                                      |
| FDivUnit          | number | Required | Forecast Distributions Per Unit (REIT)                                                                                                                  |
| FDivTotalAnn      | number | Required | Forecast Total Dividend Paid Annual                                                                                                                     |
| FPayoutRatioAnn   | number | Required | Forecast Payout Ratio Annual                                                                                                                            |
| NxFDiv1Q          | number | Required | Forecast Dividend Per Share Next Year 1st Quarter                                                                                                       |
| NxFDiv2Q          | number | Required | Forecast Dividend Per Share Next Year 2nd Quarter                                                                                                       |
| NxFDiv3Q          | number | Required | Forecast Dividend Per Share Next Year 3rd Quarter                                                                                                       |
| NxFDivFY          | number | Required | Forecast Dividend Per Share Next Year Fiscal Year End                                                                                                   |
| NxFDivAnn         | number | Required | Forecast Dividend Per Share Next Year Annual                                                                                                            |
| NxFDivUnit        | number | Required | Forecast Distributions Per Unit Next Year (REIT)                                                                                                        |
| NxFPayoutRatioAnn | number | Required | Forecast Payout Ratio Next Year Annual                                                                                                                  |
| FSales2Q          | number | Required | Forecast Net Sales 2nd Quarter                                                                                                                          |
| FOP2Q             | number | Required | Forecast Operating Profit 2nd Quarter                                                                                                                   |
| FOdP2Q            | number | Required | Forecast Ordinary Profit 2nd Quarter                                                                                                                    |
| FNP2Q             | number | Required | Forecast Profit 2nd Quarter                                                                                                                             |
| FEPS2Q            | number | Required | Forecast Earnings Per Share 2nd Quarter                                                                                                                 |
| NxFSales2Q        | number | Required | Forecast Net Sales Next Year 2nd Quarter                                                                                                                |
| NxFOP2Q           | number | Required | Forecast Operating Profit Next Year 2nd Quarter                                                                                                         |
| NxFOdP2Q          | number | Required | Forecast Ordinary Profit Next Year 2nd Quarter                                                                                                          |
| NxFNp2Q           | number | Required | Forecast Profit Next Year 2nd Quarter                                                                                                                   |
| NxFEPS2Q          | number | Required | Forecast Earnings Per Share Next Year 2nd Quarter                                                                                                       |
| FSales            | number | Required | Forecast Net Sales Fiscal Year End                                                                                                                      |
| FOP               | number | Required | Forecast Operating Profit Fiscal Year End                                                                                                               |
| FOdP              | number | Required | Forecast Ordinary Profit Fiscal Year End                                                                                                                |
| FNP               | number | Required | Forecast Profit Fiscal Year End                                                                                                                         |
| FEPS              | number | Required | Forecast Earnings Per Share Fiscal Year End                                                                                                             |
| NxFSales          | number | Required | Forecast Net Sales Next Fiscal Year End                                                                                                                 |
| NxFOP             | number | Required | Forecast Operating Profit Next Fiscal Year End                                                                                                          |
| NxFOdP            | number | Required | Forecast Ordinary Profit Next Fiscal Year End                                                                                                           |
| NxFNp             | number | Required | Forecast Profit Next Fiscal Year End                                                                                                                    |
| NxFEPS            | number | Required | Forecast Earnings Per Share Next Fiscal Year End                                                                                                        |
| MatChgSub         | string | Required | Material Changes in Subsidiaries                                                                                                                        |
| SigChgInC         | string | Required | Significant Changes In The Scope Of Consolidation  If the specified date is before 2024-07-21, the response does not contain a value for that item. |
| ChgByASRev        | string | Required | Changes Based on Revisions of Accounting Standard                                                                                                       |
| ChgNoASRev        | string | Required | Changes Other Than Ones Based on Revisions of Accounting Standard                                                                                       |
| ChgAcEst          | string | Required | Changes in Accounting Estimates                                                                                                                         |
| RetroRst          | string | Required | Retrospective Restatement                                                                                                                               |
| ShOutFY           | number | Required | Number of Issued and Outstanding Shares at Fiscal Year End Including Treasury Stock                                                                     |
| TrShFY            | number | Required | Number of Treasury Stock at Fiscal Year End                                                                                                             |
| AvgSh             | number | Required | Average Number of Shares                                                                                                                                |
| NCSales           | number | Required | Non-consolidated Net Sales                                                                                                                              |
| NCOP              | number | Required | Non-consolidated Operating Profit                                                                                                                       |
| NCOdP             | number | Required | Non-consolidated Ordinary Profit                                                                                                                        |
| NCNP              | number | Required | Non-consolidated Profit                                                                                                                                 |
| NCEPS             | number | Required | Non-consolidated Earnings Per Share                                                                                                                     |
| NCTA              | number | Required | Non-consolidated Total Assets                                                                                                                           |
| NCEq              | number | Required | Non-consolidated Equity                                                                                                                                 |
| NCEqAR            | number | Required | Non-consolidated Equity to Asset Ratio                                                                                                                  |
| NCBPS             | number | Required | Non-consolidated Book Value Per Share                                                                                                                   |
| FNCSales2Q        | number | Required | Non-consolidated Forecast Net Sales 2nd Quarter                                                                                                         |
| FNCOP2Q           | number | Required | Non-consolidated Forecast Operating Profit 2nd Quarter                                                                                                  |
| FNCOdP2Q          | number | Required | Non-consolidated Forecast Ordinary Profit 2nd Quarter                                                                                                   |
| FNCNP2Q           | number | Required | Non-consolidated Forecast Profit 2nd Quarter                                                                                                            |
| FNCEPS2Q          | number | Required | Non-consolidated Forecast Earnings Per Share 2nd Quarter                                                                                                |
| NxFNCSales2Q      | number | Required | Non-consolidated Forecast Net Sales Next Year 2nd Quarter                                                                                               |
| NxFNCOP2Q         | number | Required | Non-consolidated Forecast Operating Profit Next Year 2nd Quarter                                                                                        |
| NxFNCOdP2Q        | number | Required | Non-consolidated Forecast Ordinary Profit Next Year 2nd Quarter                                                                                         |
| NxFNCNP2Q         | number | Required | Non-consolidated Forecast Profit Next Year 2nd Quarter                                                                                                  |
| NxFNCEPS2Q        | number | Required | Non-consolidated Forecast Earnings Per Share Next Year 2nd Quarter                                                                                      |
| FNCSales          | number | Required | Non-consolidated Forecast Net Sales Fiscal Year End                                                                                                     |
| FNCOP             | number | Required | Non-consolidated Forecast Operating Profit Fiscal Year End                                                                                              |
| FNCOdP            | number | Required | Non-consolidated Forecast Ordinary Profit Fiscal Year End                                                                                               |
| FNCNP             | number | Required | Non-consolidated Forecast Profit Fiscal Year End                                                                                                        |
| FNCEPS            | number | Required | Non-consolidated Forecast Earnings Per Share Fiscal Year End                                                                                            |
| NxFNCSales        | number | Required | Non-consolidated Forecast Net Sales Next Fiscal Year End                                                                                                |
| NxFNCOP           | number | Required | Non-consolidated Forecast Operating Profit Next Fiscal Year End                                                                                         |
| NxFNCOdP          | number | Required | Non-consolidated Forecast Ordinary Profit Next Fiscal Year End                                                                                          |
| NxFNCNP           | number | Required | Non-consolidated Forecast Profit Next Fiscal Year End                                                                                                   |
| NxFNCEPS          | number | Required | Non-consolidated Forecast Earnings Per Share Next Fiscal Year End                                                                                       |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "DiscDate": "2023-01-30",
            "DiscTime": "12:00:00",
            "Code": "86970",
            "DiscNo": "20230127594871",
            "DocType": "3QFinancialStatements_Consolidated_IFRS",
            "CurPerType": "3Q",
            "CurPerSt": "2022-04-01",
            "CurPerEn": "2022-12-31",
            "CurFYSt": "2022-04-01",
            "CurFYEn": "2023-03-31",
            "NxtFYSt": "",
            "NxtFYEn": "",
            "Sales": "100529000000",
            "OP": "51765000000",
            "OdP": "",
            "NP": "35175000000",
            "EPS": "66.76",
            "DEPS": "",
            "TA": "79205861000000",
            "Eq": "320021000000",
            "EqAR": "0.004",
            "BPS": "",
            "CFO": "",
            "CFI": "",
            "CFF": "",
            "CashEq": "91135000000",
            "Div1Q": "",
            "Div2Q": "26.0",
            "Div3Q": "",
            "DivFY": "",
            "DivAnn": "",
            "DivUnit": "",
            "DivTotalAnn": "",
            "PayoutRatioAnn": "",
            "FDiv1Q": "",
            "FDiv2Q": "",
            "FDiv3Q": "",
            "FDivFY": "36.0",
            "FDivAnn": "62.0",
            "FDivUnit": "",
            "FDivTotalAnn": "",
            "FPayoutRatioAnn": "",
            "NxFDiv1Q": "",
            "NxFDiv2Q": "",
            "NxFDiv3Q": "",
            "NxFDivFY": "",
            "NxFDivAnn": "",
            "NxFDivUnit": "",
            "NxFPayoutRatioAnn": "",
            "FSales2Q": "",
            "FOP2Q": "",
            "FOdP2Q": "",
            "FNP2Q": "",
            "FEPS2Q": "",
            "NxFSales2Q": "",
            "NxFOP2Q": "",
            "NxFOdP2Q": "",
            "NxFNp2Q": "",
            "NxFEPS2Q": "",
            "FSales": "132500000000",
            "FOP": "65500000000",
            "FOdP": "",
            "FNP": "45000000000",
            "FEPS": "85.42",
            "NxFSales": "",
            "NxFOP": "",
            "NxFOdP": "",
            "NxFNp": "",
            "NxFEPS": "",
            "MatChgSub": "false",
            "SigChgInC": "",
            "ChgByASRev": "false",
            "ChgNoASRev": "false",
            "ChgAcEst": "true",
            "RetroRst": "",
            "ShOutFY": "528578441",
            "TrShFY": "1861043",
            "AvgSh": "526874759",
            "NCSales": "",
            "NCOP": "",
            "NCOdP": "",
            "NCNP": "",
            "NCEPS": "",
            "NCTA": "",
            "NCEq": "",
            "NCEqAR": "",
            "NCBPS": "",
            "FNCSales2Q": "",
            "FNCOP2Q": "",
            "FNCOdP2Q": "",
            "FNCNP2Q": "",
            "FNCEPS2Q": "",
            "NxFNCSales2Q": "",
            "NxFNCOP2Q": "",
            "NxFNCOdP2Q": "",
            "NxFNCNP2Q": "",
            "NxFNCEPS2Q": "",
            "FNCSales": "",
            "FNCOP": "",
            "FNCOdP": "",
            "FNCNP": "",
            "FNCEPS": "",
            "NxFNCSales": "",
            "NxFNCOP": "",
            "NxFNCOdP": "",
            "NxFNCNP": "",
            "NxFNCEPS": ""
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Financial Statement Data (BS/PL/CF) (/fins/details)

`GET`    /v2/fins/details

## Overview

You can obtain the entries in the balance sheet, income statement, and cash flow statement of a listed companies in their quarterly financial information.

## Attention

> **Info**
>
> - **About FinancialStatement (Various items in the financial statements)**
>   - Contents of this API is created from the EDINET XBRL taxonomy body (label information).
>   - For verbose labels (English) included in the item "FinancialStatement", see the following website.\
>     [https://disclosure2dl.edinet-fsa.go.jp/guide/static/disclosure/WEEK0060.html](https://disclosure2dl.edinet-fsa.go.jp/guide/static/disclosure/WEEK0060.html) \
>     The "Account Title List" (Accounting Standards: Japanese GAAP) and the "Taxonomy Element List for Designated International Accounting Standards" (Accounting Standards: IFRS) are available on the EDINET Taxonomy page published by fiscal year. The following data is provided for each accounting standard.
>     - If the accounting standard is Japanese GAAP, the data is provided as a set with the value of "Verbose Labels (English)" in column E of each sheet of the "Account Title List" as the key.
>     - If the accounting standard is IFRS, the data is provided as a set with the value of "Verbose Labels (English)" in column D of each sheet of the "Taxonomy Element List for Designated International Accounting Standards" as the key.
> - **About Taxonomy Extension**
>   - Company-specific items defined in the taxonomy by submitter that do not exist in the EDINET taxonomy are not covered by this API.

> **Note**
>
> - MODEC, Inc. (stock code 62690) presents its consolidated financial statements and notes to consolidated financial statements in U.S. dollars in its financial statements for February 2022 and thereafter. Therefore, the financial statement information for the subject issue in this service is also provided in U.S. dollars.

## Get quarterly financial statement information

`GET` `https://api.jquants.com/v2/fins/details`

Either "code" or "date" must be specified.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| code            | string | Optional | Issue code (e.g. 86970 or 8697)  4 or 5 character issue code                                                                                         |
| date            | string | Optional | Disclosure date (e.g. 2022-01-05 or 20220105)                                                                                                            |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/fins/details

**cURL**

```bash
curl -G https://api.jquants.com/v2/fins/details \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: 'https://api.jquants.com',
headers: { 'x-api-key': '{{apiKey}}' },
})

await client.get('/v2/fins/details', {
params: {
  code: '{{code}}',
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/fins/details",
  params={"code": "{{code}}", "date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                                                                                                                                                                                                                             |
| --------- | ------ | -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| DiscDate  | string | Required | Disclosed Date                                                                                                                                                                                                                          |
| DiscTime  | string | Required | Disclosed Time                                                                                                                                                                                                                          |
| Code      | string | Required | Issue Code (5 digits)                                                                                                                                                                                                                   |
| DiscNo    | string | Required | Disclosure Number  The json output from the API is sorted in ascending order by disclosure number.                                                                                                                                  |
| DocType   | string | Required | Type of Document  [Type of Document List](/en/spec/fin-summary/typeofdocument)                                                                                                                                                      |
| FS        | object | Required | Various items in financial statements  Data stored with verbose label (English) as key and its value (financial statement value) as value.  Redundant labels (English) associated with XBRL tags and their values are recorded. |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "DiscDate": "2020-04-30",
            "DiscTime": "12:00:00",
            "Code": "86970",
            "DiscNo": "20200429402226",
            "DocType": "FYFinancialStatements_Consolidated_IFRS",
            "FS": {
                "EDINET code, DEI": "E03814",
                "Security code, DEI": "86970",
                "Filer name in Japanese, DEI": "株式会社日本取引所グループ",
                "Filer name in English, DEI": "Japan Exchange Group, Inc.",
                "Document type, DEI": "通期第３号参考様式　[IFRS]（連結）",
                "Accounting standards, DEI": "IFRS",
                "Whether consolidated financial statements are prepared, DEI": "true",
                "Industry code when consolidated financial statements are prepared in accordance with industry specific regulations, DEI": "CTE",
                "Industry code when financial statements are prepared in accordance with industry specific regulations, DEI": "CTE",
                "Current fiscal year start date, DEI": "2019-04-01",
                "Current period end date, DEI": "2020-03-31",
                "Type of current period, DEI": "FY",
                "Current fiscal year end date, DEI": "2020-03-31",
                "Previous fiscal year start date, DEI": "2018-04-01",
                "Comparative period end date, DEI": "2019-03-31",
                "Previous fiscal year end date, DEI": "2019-03-31",
                "Amendment flag, DEI": "false",
                "Report amendment flag, DEI": "false",
                "XBRL amendment flag, DEI": "false",
                "Cash and cash equivalents (IFRS)": "71883000000",
                "Trade and other receivables - CA (IFRS)": "16686000000",
                "Income taxes receivable - CA (IFRS)": "5922000000",
                "Other financial assets - CA (IFRS)": "117400000000",
                "Other current assets - CA (IFRS)": "1837000000",
                "Current assets (IFRS)": "67093263000000",
                "Property, plant and equipment (IFRS)": "14798000000",
                "Goodwill (IFRS)": "67374000000",
                "Intangible assets (IFRS)": "35045000000",
                "Retirement benefit asset - NCA (IFRS)": "5642000000",
                "Investments accounted for using equity method (IFRS)": "14703000000",
                "Other financial assets - NCA (IFRS)": "18156000000",
                "Other non-current assets - NCA (IFRS)": "6049000000",
                "Deferred tax assets (IFRS)": "3321000000",
                "Non-current assets (IFRS)": "193039000000",
                "Assets (IFRS)": "67286302000000",
                "Trade and other payables - CL (IFRS)": "6643000000",
                "Bonds and borrowings - CL (IFRS)": "32500000000",
                "Income taxes payable - CL (IFRS)": "10289000000",
                "Other current liabilities - CL (IFRS)": "10062000000",
                "Current liabilities (IFRS)": "66947278000000",
                "Bonds and borrowings - NCL (IFRS)": "19953000000",
                "Retirement benefit liability - NCL (IFRS)": "8866000000",
                "Other non-current liabilities - NCL (IFRS)": "2162000000",
                "Deferred tax liabilities (IFRS)": "2665000000",
                "Non-current liabilities (IFRS)": "33648000000",
                "Liabilities (IFRS)": "66980926000000",
                "Share capital (IFRS)": "11500000000",
                "Capital surplus (IFRS)": "39716000000",
                "Treasury shares (IFRS)": "-1548000000",
                "Other components of equity (IFRS)": "5602000000",
                "Retained earnings (IFRS)": "242958000000",
                "Equity attributable to owners of parent (IFRS)": "298228000000",
                "Non-controlling interests (IFRS)": "7146000000",
                "Equity (IFRS)": "305375000000",
                "Liabilities and equity (IFRS)": "67286302000000",
                "Number of submission, DEI": "1",
                "Profit (loss) before tax from continuing operations (IFRS)": "69095000000.0",
                "Depreciation and amortization - OpeCF (IFRS)": "16499000000",
                "Finance income - OpeCF (IFRS)": "-665000000",
                "Finance costs - OpeCF (IFRS)": "96000000",
                "Share of loss (profit) of investments accounted for using equity method - OpeCF (IFRS)": "-2457000000",
                "Decrease (increase) in trade and other receivables - OpeCF (IFRS)": "-5246000000",
                "Increase (decrease) in trade and other payables - OpeCF (IFRS)": "420000000",
                "Decrease (increase) in retirement benefit asset - OpeCF (IFRS)": "230000000",
                "Increase (decrease) in retirement benefit liability - OpeCF (IFRS)": "12000000",
                "Other, Changes in working capital - OpeCF (IFRS)": "-424000000",
                "Subtotal - OpeCF (IFRS)": "77560000000",
                "Interest and dividends received - OpeCF (IFRS)": "899000000",
                "Interest paid - OpeCF (IFRS)": "-96000000",
                "Income taxes refund (paid) - OpeCF (IFRS)": "-21482000000",
                "Net cash provided by (used in) operating activities (IFRS)": "56881000000",
                "Payments into time deposits - InvCF (IFRS)": "-117400000000",
                "Proceeds from withdrawal of time deposits - InvCF (IFRS)": "113100000000",
                "Purchase of property, plant and equipment - InvCF (IFRS)": "-1199000000",
                "Purchase of intangible assets - InvCF (IFRS)": "-12379000000",
                "Proceeds from sale of investment securities - InvCF (IFRS)": "11585000000",
                "Payments for acquisition of subsidiaries - InvCF (IFRS)": "-3165000000",
                "Other - InvCF (IFRS)": "23000000",
                "Net cash provided by (used in) investing activities (IFRS)": "-9434000000",
                "Repayments of lease liabilities - FinCF (IFRS)": "-3125000000",
                "Dividends paid - FinCF (IFRS)": "-35935000000",
                "Purchase of treasury shares - FinCF (IFRS)": "-350000000",
                "Net cash provided by (used in) financing activities (IFRS)": "-39411000000",
                "Net increase (decrease) in cash and cash equivalents before effect of exchange rate changes (IFRS)": "8035000000",
                "Effect of exchange rate changes on cash and cash equivalents (IFRS)": "-43000000",
                "Other income (IFRS)": "975000000.0",
                "Revenue - 2 (IFRS)": "124663000000.0",
                "Operating expenses (IFRS)": "58532000000.0",
                "Other expenses (IFRS)": "54000000.0",
                "Share of profit (loss) of investments accounted for using equity method (IFRS)": "2457000000.0",
                "Operating profit (loss) (IFRS)": "68533000000.0",
                "Finance income (IFRS)": "665000000.0",
                "Finance costs (IFRS)": "103000000.0",
                "Income tax expense (IFRS)": "20781000000.0",
                "Profit (loss) (IFRS)": "48314000000.0",
                "Profit (loss) attributable to owners of parent (IFRS)": "47609000000.0",
                "Profit (loss) attributable to non-controlling interests (IFRS)": "705000000.0",
                "Basic earnings (loss) per share (IFRS)": "88.91"
            }
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Cash Dividend Data (/fins/dividend)

`GET`    /v2/fins/dividend

## Overview

Provides information on dividends (determined and forecast) per share of listed companies, record date, ex-rights date, and payable date.

## Attention

> **Info**
>
> - Stocks that are not listed on the TSE (including issue listed only on the other exchanges) are not included in the data.

## Get dividend data

`GET` `https://api.jquants.com/v2/fins/dividend`

Either issue code (code) or date (date) must be specified.

### Parameter and Response

Either issue code (code) or date (date) must be specified.\
The combination of each parameter and the results of the response are as below.

- code: ✓, date: –, from /to: – → Cash dividend data for all available period.

- code: ✓, date: ✓, from /to: – → Cash dividend data for a specific issue on the specific date.

- code: ✓, date: –, from /to: ✓ → Cash dividend data for a specific issue for the specified period.

- code: –, date: ✓, from /to: – → Cash dividend data for all listed issues on the specific date.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> Either **code** or **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                                                                            |
| --------------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| code            | string | Optional | Issue code  (e.g. 27800 or 2780)  If a 4-character issue code is specified, only the data of common stock will be obtained for the issue on which both common and preferred stocks are listed. |
| from            | string | Optional | Starting point of data period (e.g. 20210901 or 2021-09-01)                                                                                                                                            |
| to              | string | Optional | End point of data period (e.g. 20210907 or 2021-09-07)                                                                                                                                                 |
| date            | string | Optional | When "from" and "to" are not specified (e.g. 20210907 or 2021-09-07)                                                                                                                                   |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation.                                               |

### Sample Code

/v2/fins/dividend

**cURL**

```bash
curl -G https://api.jquants.com/v2/fins/dividend \
-H "x-api-key: {{apiKey}}" \
-d code="{{code}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/fins/dividend", {
params: {
  code: '{{code}}',
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/fins/dividend",
  params={"code": "{{code}}", "date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter        | Type            | Required | Description                                                                                                                                                                                                                   |
| ---------------- | --------------- | -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| PubDate          | string          | Required | Announcement Date (YYYY-MM-DD)                                                                                                                                                                                                |
| PubTime          | string          | Required | Announcement Time (HH:MM)                                                                                                                                                                                                     |
| Code             | string          | Required | Issue code                                                                                                                                                                                                                    |
| RefNo            | string          | Required | Reference Number  Number to uniquely identify the dividend notification  See [about Reference Number](/en/spec/fin-dividend/reference-number)                                                                         |
| StatCode         | string          | Required | Status Code  1: new, 2: revised, 3: delete                                                                                                                                                                                |
| BoardDate        | string          | Required | Date of Board of Directors' resolution                                                                                                                                                                                        |
| IFCode           | string          | Required | Interim/Final Code  1: interim, 2: final                                                                                                                                                                                  |
| FRCode           | string          | Required | Forecast/Result Code  1: result, 2: forecast                                                                                                                                                                              |
| IFTerm           | string          | Required | Interim Final Term                                                                                                                                                                                                            |
| DivRate          | number / string | Required | Dividend value per share  "-" if undetermined, "" if not applicable.                                                                                                                                                      |
| RecDate          | string          | Required | Record date                                                                                                                                                                                                                   |
| ExDate           | string          | Required | Ex-rights date                                                                                                                                                                                                                |
| ActRecDate       | string          | Required | Date of Dividend Vesting                                                                                                                                                                                                      |
| PayDate          | string          | Required | Scheduled payment start date  "-" if undetermined, "" if not applicable.                                                                                                                                                  |
| CARefNo          | string          | Required | CA Reference Number  Reference number of the dividend notice of modification or deletion. For new notification, same value as Reference number.  See [about Reference Number](/en/spec/fin-dividend/reference-number) |
| DistAmt          | number / string | Required | Amount of cash delivered per share  "-" if undetermined, "" if not applicable.  Provides only after February 24, 2014.                                                                                                |
| RetEarn          | number / string | Required | Retained earnings per share  "-" if undetermined, "" if not applicable.  Provides only after February 24, 2014.                                                                                                       |
| DeemDiv          | number / string | Required | Deemed dividend per share  "-" if undetermined, "" if not applicable.  Provides only after February 24, 2014.                                                                                                         |
| DeemCapGains     | number / string | Required | Amount of deemed transfer income per share  "-" if undetermined, "" if not applicable.  Provides only after February 24, 2014.                                                                                        |
| NetAssetDecRatio | number / string | Required | Decrease ratio in net assets  "-" if undetermined, "" if not applicable.  Provides only after February 24, 2014.                                                                                                      |
| CommSpecCode     | string          | Required | Code stands for Commemorative/Special dividend  1: Commemorative, 2: Special, 3: Both, 0: Normal                                                                                                                          |
| CommDivRate      | number / string | Required | Commemorative dividend value per share  "-" if undetermined, "" if not applicable.  Provides only after June 6, 2022.                                                                                                 |
| SpecDivRate      | number / string | Required | Special dividend value per share  "-" if undetermined, "" if not applicable.  Provides only after June 6, 2022.                                                                                                       |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "PubDate": "2014-02-24",
            "PubTime": "09:21",
            "Code": "15550",
            "RefNo": "201402241B00002",
            "StatCode": "1",
            "BoardDate": "2014-02-24",
            "IFCode": "2",
            "FRCode": "2",
            "IFTerm": "2014-03",
            "DivRate": "-",
            "RecDate": "2014-03-10",
            "ExDate": "2014-03-06",
            "ActRecDate": "2014-03-10",
            "PayDate": "-",
            "CARefNo": "201402241B00002",
            "DistAmt": "",
            "RetEarn": "",
            "DeemDiv": "",
            "DeemCapGains": "",
            "NetAssetDecRatio": "",
            "CommSpecCode": "0",
            "CommDivRate": "",
            "SpecDivRate": ""
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Earnings Calendar (/equities/earnings-calendar)

`GET`    /v2/equities/earnings-calendar

## Overview

This API provides the announcement date of financial results. For now, companies with fiscal year ends in March or September can be obtained. (Companies with fiscal year ends in other month will be supported in the future.)

## Attention

> **Info**
>
> - It will be updated at around 19:00 (JST) only when there is an update for companies which end their fiscal year in March or September at the following site. If there are no updates for companies which end their fiscal year in March or September, the data as of the last update is provided by this API.\
>   [https://www.jpx.co.jp/english/listing/event-schedules/financial-announcement/index.html](https://www.jpx.co.jp/english/listing/event-schedules/financial-announcement/index.html)
> - This API returns information about stocks whose financial results will be announced on the next business day.
> - If there is no record with the next business day in the data obtained from the API, it means that there are no companies scheduled to disclose on the next business day among the companies with fiscal year ends in March or September.
> - REIT data is not included.

## Inquire the issue code, fiscal year, and quarter scheduled to be announced.

`GET` `https://api.jquants.com/v2/equities/earnings-calendar`

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/equities/earnings-calendar

**cURL**

```bash
curl -G https://api.jquants.com/v2/equities/earnings-calendar \
-H "x-api-key: {{apiKey}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/equities/earnings-calendar")
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/equities/earnings-calendar",
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter | Type   | Required | Description                                                                                                   |
| --------- | ------ | -------- | ------------------------------------------------------------------------------------------------------------- |
| Date      | string | Required | Date (YYYY-MM-DD)  If the earnings announcement date is undecided, the data will be an empty string (""). |
| Code      | string | Required | Issue code                                                                                                    |
| CoName    | string | Required | Company name (Japanese)                                                                                       |
| FY        | string | Required | End of Fiscal year (Japanese)                                                                                 |
| SectorNm  | string | Required | Sector name (Japanese)                                                                                        |
| FQ        | string | Required | Fiscal quarter (Japanese)                                                                                     |
| Section   | string | Required | Market segment name (Japanese)                                                                                |

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2022-02-14",
            "Code": "43760",
            "CoName": "くふうカンパニー",
            "FY": "9月30日",
            "SectorNm": "情報・通信業",
            "FQ": "第１四半期",
            "Section": "マザーズ"
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Index Option Prices (OHLC) (/derivatives/bars/daily/options/225)

`GET`    /v2/derivatives/bars/daily/options/225

## Overview

Information on the OHLC, settlement price, and theoretical price of Nikkei 225 Options can be obtained through this API.\
The data that can be obtained is only for Nikkei 225 Index Options (excluding Weekly Options and Flexible options).

## Attention

> **Info**
>
> - **About Trading Session**
>   - Prior to February 10, 2011, Trading session consists of the night session, the morning session, and the afternoon session.
>   - Morning session data for this period is not recorded, and afternoon session data is recorded as day session data. (Note that the whole day data reflects all sessions.)
>   - After February 14, 2011, Trading session consists of the night session and the day session.
> - **About key items in response**
>   - When emergency margin is triggered, data as of both the clearing price calculation and the emergency margin calculation are generated for the same trading day and issue. Therefore, it is possible to uniquely identify the record by combining Date, Code and EmMrgnTrgDiv (EmergencyMarginTriggerDivision).

## Get daily Nikkei 225 Options prices (OHLC)

`GET` `https://api.jquants.com/v2/derivatives/bars/daily/options/225`

"date" must be specified.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| date            | string | Required | Date (e.g. 20210901 or 2021-09-01)                                                                                                                       |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/derivatives/bars/daily/options/225

**cURL**

```bash
curl -G https://api.jquants.com/v2/derivatives/bars/daily/options/225 \
-H "x-api-key: {{apiKey}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/derivatives/bars/daily/options/225", {
params: {
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/derivatives/bars/daily/options/225",
  params={"date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter    | Type            | Required | Description                                                                                                                                                                                                      |
| ------------ | --------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Date         | string          | Required | Trading day (YYYY-MM-DD)                                                                                                                                                                                         |
| Code         | string          | Required | Issue code                                                                                                                                                                                                       |
| O            | number          | Required | Open price (whole day)                                                                                                                                                                                           |
| H            | number          | Required | High price (whole day)                                                                                                                                                                                           |
| L            | number          | Required | Low price (whole day)                                                                                                                                                                                            |
| C            | number          | Required | Close price (whole day)                                                                                                                                                                                          |
| EO           | number / string | Required | Open price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                         |
| EH           | number / string | Required | High price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                         |
| EL           | number / string | Required | Low price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                          |
| EC           | number / string | Required | Close price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                        |
| AO           | number          | Required | Open price (day session)                                                                                                                                                                                         |
| AH           | number          | Required | High price (day session)                                                                                                                                                                                         |
| AL           | number          | Required | Low price (day session)                                                                                                                                                                                          |
| AC           | number          | Required | Close price (day session)                                                                                                                                                                                        |
| Vo           | number          | Required | Volume                                                                                                                                                                                                           |
| OI           | number          | Required | Open interest                                                                                                                                                                                                    |
| Va           | number          | Required | Trading value                                                                                                                                                                                                    |
| CM           | string          | Required | Contract month (YYYY-MM)                                                                                                                                                                                         |
| Strike       | number          | Required | Strike price                                                                                                                                                                                                     |
| VoOA         | number          | Required | Volume (only auction) (\*1)                                                                                                                                                                                      |
| EmMrgnTrgDiv | string          | Required | Emergency margin trigger division  001: When emergency margin is triggered, 002: When settlement price is calculated.  "001" is recorded only if the emergency margin was triggered after July 19, 2016. |
| PCDiv        | string          | Required | Put Call division  1: Put, 2: Call                                                                                                                                                                           |
| LTD          | string          | Required | Last trading day (YYYY-MM-DD) (\*1)                                                                                                                                                                              |
| SQD          | string          | Required | Special quotation day (YYYY-MM-DD) (\*1)                                                                                                                                                                         |
| Settle       | number          | Required | Settlement price (\*1)                                                                                                                                                                                           |
| Theo         | number          | Required | Theoretical price (\*1)                                                                                                                                                                                          |
| BaseVol      | number          | Required | Base volatility  Average of the implied volatility of at-the-money put and call (\*1)                                                                                                                        |
| UnderPx      | number          | Required | Underlying price (\*1)                                                                                                                                                                                           |
| IV           | number          | Required | Implied volatility (\*1)                                                                                                                                                                                         |
| IR           | number          | Required | Interest rate for theoretical price calculation (\*1)                                                                                                                                                            |

\*1 Data after July 19, 2016 contains value for these fields.

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Date": "2023-03-22",
            "Code": "130060018",
            "O": 0.0,
            "H": 0.0,
            "L": 0.0,
            "C": 0.0,
            "EO": 0.0,
            "EH": 0.0,
            "EL": 0.0,
            "EC": 0.0,
            "AO": 0.0,
            "AH": 0.0,
            "AL": 0.0,
            "AC": 0.0,
            "Vo": 0.0,
            "OI": 330.0,
            "Va": 0.0,
            "CM": "2025-06",
            "Strike": 20000.0,
            "VoOA": 0.0,
            "EmMrgnTrgDiv": "002",
            "PCDiv": "1",
            "LTD": "2025-06-12",
            "SQD": "2025-06-13",
            "Settle": 980.0,
            "Theo": 974.641,
            "BaseVol": 17.93025,
            "UnderPx": 27466.61,
            "IV": 23.1816,
            "IR": 0.2336
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Futures Prices (OHLC) (/derivatives/bars/daily/futures)

`GET`    /v2/derivatives/bars/daily/futures

## Overview

Information on the OHLC, settlement price, and theoretical price of Futures can be obtained through this API.\
Please refer to [Derivative Product Category Codes](/en/spec/drv-bars-daily-fut/derivative-product-category) for the data that can be obtained.

## Attention

> **Info**
>
> - **About Issue Code**
>   - Please refer to [Securities Code Related Materials](https://www.jpx.co.jp/english/sicc/securities-code/01.html) for the numbering rules of futures and options trading identification codes.
> - **About Trading Session**
>   - Prior to February 10, 2011, Trading session consists of the night session, the morning session, and the afternoon session.
>   - Morning session data for this period is not recorded, and afternoon session data is recorded as day session data. (Note that the whole day data reflects all sessions.)
>   - After February 14, 2011, Trading session consists of the night session and the day session.
> - **About Holiday Trading**
>   - Trading days for holiday trading are treated as the same trading day as the night session that starts on the weekday immediately preceding the holiday (business day before the holiday) and the day session on the weekday immediately following the holiday (business day after the holiday).
> - **About key items in response**
>   - When emergency margin is triggered, data as of both the clearing price calculation and the emergency margin calculation are generated for the same trading day and issue. Therefore, it is possible to uniquely identify the record by combining Date, Code and EmMrgnTrgDiv (EmergencyMarginTriggerDivision).

## Get daily Futures prices (OHLC)

`GET` `https://api.jquants.com/v2/derivatives/bars/daily/futures`

"date" must be specified.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| category        | string | Optional | Derivative Product Category                                                                                                                              |
| date            | string | Required | Date (e.g. 20210901 or 2021-09-01)                                                                                                                       |
| contract\_flag  | string | Optional | Central contract month flag                                                                                                                              |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/derivatives/bars/daily/futures

**cURL**

```bash
curl -G https://api.jquants.com/v2/derivatives/bars/daily/futures \
-H "x-api-key: {{apiKey}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/derivatives/bars/daily/futures", {
params: {
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/derivatives/bars/daily/futures",
  params={"date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter    | Type            | Required | Description                                                                                                                                                                                                      |
| ------------ | --------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Code         | string          | Required | Issue code                                                                                                                                                                                                       |
| ProdCat      | string          | Required | Derivative Product Category                                                                                                                                                                                      |
| Date         | string          | Required | Trading day (YYYY-MM-DD)                                                                                                                                                                                         |
| O            | number          | Required | Open price (whole day)                                                                                                                                                                                           |
| H            | number          | Required | High price (whole day)                                                                                                                                                                                           |
| L            | number          | Required | Low price (whole day)                                                                                                                                                                                            |
| C            | number          | Required | Close price (whole day)                                                                                                                                                                                          |
| MO           | number / string | Required | Open price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                          |
| MH           | number / string | Required | High price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                          |
| ML           | number / string | Required | Low price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                           |
| MC           | number / string | Required | Close price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                         |
| EO           | number / string | Required | Open price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                         |
| EH           | number / string | Required | High price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                         |
| EL           | number / string | Required | Low price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                          |
| EC           | number / string | Required | Close price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                        |
| AO           | number          | Required | Open price (day session)                                                                                                                                                                                         |
| AH           | number          | Required | High price (day session)                                                                                                                                                                                         |
| AL           | number          | Required | Low price (day session)                                                                                                                                                                                          |
| AC           | number          | Required | Close price (day session)                                                                                                                                                                                        |
| Vo           | number          | Required | Volume                                                                                                                                                                                                           |
| OI           | number          | Required | Open interest                                                                                                                                                                                                    |
| Va           | number          | Required | Trading value                                                                                                                                                                                                    |
| CM           | string          | Required | Contract month (YYYY-MM)                                                                                                                                                                                         |
| VoOA         | number          | Required | Volume (only auction) (\*1)                                                                                                                                                                                      |
| EmMrgnTrgDiv | string          | Required | Emergency margin trigger division  001: When emergency margin is triggered, 002: When settlement price is calculated.  "001" is recorded only if the emergency margin was triggered after July 19, 2016. |
| LTD          | string          | Required | Last trading day (YYYY-MM-DD) (\*1)                                                                                                                                                                              |
| SQD          | string          | Required | Special quotation day (YYYY-MM-DD) (\*1)                                                                                                                                                                         |
| Settle       | number          | Required | Settlement price (\*1)                                                                                                                                                                                           |
| CCMFlag      | string          | Required | Flag of the central contract month (1: Central contract month, 0: Others) (\*1)                                                                                                                                  |

\*1 Data after July 19, 2016 contains value for these fields.

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Code": "169090005",
            "ProdCat": "TOPIXF",
            "Date": "2024-07-23",
            "O": 2825.5,
            "H": 2853.0,
            "L": 2825.5,
            "C": 2829.0,
            "MO": "",
            "MH": "",
            "ML": "",
            "MC": "",
            "EO": 2825.5,
            "EH": 2850.0,
            "EL": 2825.5,
            "EC": 2845.0,
            "AO": 2850.5,
            "AH": 2853.0,
            "AL": 2826.0,
            "AC": 2829.0,
            "Vo": 42910.0,
            "OI": 479812.0,
            "Va": 1217918971856.0,
            "CM": "2024-09",
            "VoOA": 40405.0,
            "EmMrgnTrgDiv": "002",
            "LTD": "2024-09-12",
            "SQD": "2024-09-13",
            "Settle": 2829.0,
            "CCMFlag": "1"
        }
    ],
    "pagination_key": "value1.value2."
}
```

# Options Prices (OHLC) (/derivatives/bars/daily/options)

`GET`    /v2/derivatives/bars/daily/options

You can obtain Options data (OHLC, settlement price, etc.).

## Overview

Information on the OHLC, settlement price, and theoretical price of Options can be obtained through this API.\
Please refer to [Derivative Product Category Codes](/en/spec/drv-bars-daily-opt/derivative-product-category) for the data that can be obtained.

## Attention

> **Info**
>
> - **About Issue Code**
>   - Please refer to [Securities Code Related Materials](https://www.jpx.co.jp/english/sicc/securities-code/01.html) for the numbering rules of futures and options trading identification codes.
> - **About Trading Session**
>   - Prior to February 10, 2011, Trading session consists of the night session, the morning session, and the afternoon session.
>   - Morning session data for this period is not recorded, and afternoon session data is recorded as day session data. (Note that the whole day data reflects all sessions.)
>   - After February 14, 2011, Trading session consists of the night session and the day session.
> - **About Holiday Trading**
>   - Trading days for holiday trading are treated as the same trading day as the night session that starts on the weekday immediately preceding the holiday (business day before the holiday) and the day session on the weekday immediately following the holiday (business day after the holiday).
> - **About key items in response**
>   - When emergency margin is triggered, data as of both the clearing price calculation and the emergency margin calculation are generated for the same trading day and issue. Therefore, it is possible to uniquely identify the record by combining Date, Code and EmMrgnTrgDiv (EmergencyMarginTriggerDivision).

## Get daily Options prices (OHLC)

`GET` `https://api.jquants.com/v2/derivatives/bars/daily/options`

"date" must be specified.

### Requests

### Headers

| Parameter | Type   | Required | Description |
| --------- | ------ | -------- | ----------- |
| x-api-key | string | Required | API Key     |

### Query Parameters

> **Note**
>
> **date** must be specified.

| Parameter       | Type   | Required | Description                                                                                                                                              |
| --------------- | ------ | -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| category        | string | Optional | Derivative Product Category                                                                                                                              |
| code            | string | Optional | Underlying securities code  Set when specifying securities options in category                                                                       |
| date            | string | Required | Date (e.g. 20210901 or 2021-09-01)                                                                                                                       |
| contract\_flag  | string | Optional | Central contract month flag                                                                                                                              |
| pagination\_key | string | Optional | The primary key of the first item that this operation will evaluate.  Use the value that was returned for pagination\_key in the previous operation. |

### Sample Code

/v2/derivatives/bars/daily/options

**cURL**

```bash
curl -G https://api.jquants.com/v2/derivatives/bars/daily/options \
-H "x-api-key: {{apiKey}}" \
-d date="{{date}}"
```

**JavaScript**

```javascript
import axios from 'axios'

const client = axios.create({
baseURL: "https://api.jquants.com",
headers: { "x-api-key": "{{apiKey}}" },
})

await client.get("/v2/derivatives/bars/daily/options", {
params: {
  date: '{{date}}',
},
})
```

**Python**

```python
import requests

headers = {"x-api-key": "{{apiKey}}"}
resp = requests.get(
  "https://api.jquants.com/v2/derivatives/bars/daily/options",
  params={"date": "{{date}}"},
  headers=headers,
)
print(resp.json())
```

### Responses

### Data Item

| Parameter    | Type            | Required | Description                                                                                                                                                                                                      |
| ------------ | --------------- | -------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Code         | string          | Required | Issue code                                                                                                                                                                                                       |
| ProdCat      | string          | Required | Derivative Product Category                                                                                                                                                                                      |
| UndSSO       | string          | Required | Underlying securities for securities options  "-" is set for non-securities options                                                                                                                          |
| Date         | string          | Required | Trading day (YYYY-MM-DD)                                                                                                                                                                                         |
| O            | number          | Required | Open price (whole day)                                                                                                                                                                                           |
| H            | number          | Required | High price (whole day)                                                                                                                                                                                           |
| L            | number          | Required | Low price (whole day)                                                                                                                                                                                            |
| C            | number          | Required | Close price (whole day)                                                                                                                                                                                          |
| MO           | number / string | Required | Open price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                          |
| MH           | number / string | Required | High price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                          |
| ML           | number / string | Required | Low price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                           |
| MC           | number / string | Required | Close price (morning session)  If the stock is not eligible for morning/afternoon session, a blank character is set.                                                                                         |
| EO           | number / string | Required | Open price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                         |
| EH           | number / string | Required | High price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                         |
| EL           | number / string | Required | Low price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                          |
| EC           | number / string | Required | Close price (night session)  For the issue on the first day of trading, blank is set since there is no night session.                                                                                        |
| AO           | number          | Required | Open price (day session)                                                                                                                                                                                         |
| AH           | number          | Required | High price (day session)                                                                                                                                                                                         |
| AL           | number          | Required | Low price (day session)                                                                                                                                                                                          |
| AC           | number          | Required | Close price (day session)                                                                                                                                                                                        |
| Vo           | number          | Required | Volume                                                                                                                                                                                                           |
| OI           | number          | Required | Open interest                                                                                                                                                                                                    |
| Va           | number          | Required | Trading value                                                                                                                                                                                                    |
| CM           | string          | Required | Contract month (YYYY-MM)  For Nikkei 225 mini options, it shows weeks instead of months (e.g. 2024-51 is the 51st week of 2024).                                                                             |
| Strike       | number          | Required | Strike price                                                                                                                                                                                                     |
| VoOA         | number          | Required | Volume (only auction) (\*1)                                                                                                                                                                                      |
| EmMrgnTrgDiv | string          | Required | Emergency margin trigger division  001: When emergency margin is triggered, 002: When settlement price is calculated.  "001" is recorded only if the emergency margin was triggered after July 19, 2016. |
| PCDiv        | string          | Required | Put Call division  1: Put, 2: Call                                                                                                                                                                           |
| LTD          | string          | Required | Last trading day (YYYY-MM-DD) (\*1)                                                                                                                                                                              |
| SQD          | string          | Required | Special quotation day (YYYY-MM-DD) (\*1)                                                                                                                                                                         |
| Settle       | number          | Required | Settlement price (\*1)                                                                                                                                                                                           |
| Theo         | number          | Required | Theoretical price (\*1)                                                                                                                                                                                          |
| BaseVol      | number          | Required | Base volatility (\*1)                                                                                                                                                                                            |
| UnderPx      | number          | Required | Underlying price (\*1)                                                                                                                                                                                           |
| IV           | number          | Required | Implied volatility (\*1)                                                                                                                                                                                         |
| IR           | number          | Required | Interest rate for theoretical price calculation (\*1)                                                                                                                                                            |
| CCMFlag      | string          | Required | Flag of the central contract month (1: Central contract month, 0: Others) (\*1)                                                                                                                                  |

\*1 Data after July 19, 2016 contains value for these fields.

### Response Sample

```bash {{ title: "200:OK" }}
{
    "data": [
        {
            "Code": "140014505",
            "ProdCat": "TOPIXE",
            "UndSSO": "-",
            "Date": "2024-07-23",
            "O": 0.0,
            "H": 0.0,
            "L": 0.0,
            "C": 0.0,
            "MO": "",
            "MH": "",
            "ML": "",
            "MC": "",
            "EO": 0.0,
            "EH": 0.0,
            "EL": 0.0,
            "EC": 0.0,
            "AO": 0.0,
            "AH": 0.0,
            "AL": 0.0,
            "AC": 0.0,
            "Vo": 0.0,
            "OI": 0.0,
            "Va": 0.0,
            "CM": "2025-01",
            "Strike": 2450.0,
            "VoOA": 0.0,
            "EmMrgnTrgDiv": "002",
            "PCDiv": "2",
            "LTD": "2025-01-09",
            "SQD": "2025-01-10",
            "Settle": 377.0,
            "Theo": 380.3801,
            "BaseVol": 18.115,
            "UnderPx": 2833.39,
            "IV": 17.2955,
            "IR": 0.3527,
            "CCMFlag": "0"
        }
    ],
    "pagination_key": "value1.value2."
}
```
