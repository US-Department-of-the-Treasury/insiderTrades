
# insiderTrades

<!-- badges: start -->
<!-- badges: end -->

The goal of insiderTrades is to provide functions that download insider trading transactions and insider holdings from a public NoSQL SEC database using keyword criteria and generate a relational dataframe as the output. The functions are able to handle filings from 2004Q1 to present. There are four different types of functions, those that handle non-derivative transactions, those that handle derivative transactions, those that handle non-derivative holdings, and those that handle derivative holdings.

The SEC requires insiders, who are defined as officers, directors, and those that hold more than 10% of any class of a company's securities, to report purchase sales, and holdings. The insiders report any transactions by Form 4 within two business days of the transaction. An insider must file a Form 5 within 45 days of the fiscal year end if any transactions were not reported on a Form 4 during the fiscal year because 1) exemption to filing or 2) a failure to file a Form 4. Additionally, insiders at times disclose their total holding of a company's securities.

Due to the limitations the SEC has on the number of queries per second, it takes 22-24 hours for the functions to check for keywords and wrangle the transactions into a relational dataframe. Additionally, with how the database is structured, a function can only examine the insider trading filings from a single quarter at a time.

## Examples
### Example - No keywords
How the SEC repository is structured is that the filings are organized by form type, quarter, and year. These arguments plus the name and email arguments are the only required arguments for the function. For the name and email arguments, please put your name and email address since this is required by the SEC.

``` r
library(insiderTrades)

dat1 <- insiderTrades::nonderivativeTransactionsPullAndScrape(quarter = 2, year = 2015, form = 4, name = "Your Name", email = "YourEmail@YourEmail.com")
```

### Example - Keywords
All of the functions have the ability to use key words to determine if a transaction should be included in the final dataframe. The different types of keywords are the following:

* footnoteKeywords - This criteria searches the footnote(s) associated with a transaction. The footnotes follow normal capitalization rules.
* rptOwnerKeywords - This criteria searches the reporting owners information (includes CIK, name, address, city, state, and zip code). Note that all the text is capitalized. The structure of the name is LAST FIRST M. The address information is structured into separate lines of address, city, state, and zip code.
* issuerKeywords - This criteria searches the issuers information which includes the CIK and the name of the firm. The text is all capitalized.
* issuerTradingSymbol - This criteria searches the issuer's trading symbol. The text is all capitalized.
* transactionType - This criteria searches the transaction's type. Examples include G for gifts and J for Other. A full list can be found [here](https://www.sec.gov/files/forms-3-4-5.pdf).

An important item of note is that a transaction will be included as long as it fulfills only one of the key words. Thus a good way to think about the key words is that they are connected by OR rather than by AND. Thus any transaction that contains "gift" or "charity" or "charitable" in the footnotes or contains the name "SMITH" or is a gift transaction will be included. 

``` r
dat2 <- insiderTrades::nonderivativeHoldingsPullAndScrape(quarter = 2, year = 2018, form = 4, name = "Your Name", email = "YourEmail@YourEmail.com", footnoteKeywords = c("gift", "charity", "charitable"), transactionType = "G", rptOwnerKeywords = "SMITH")
```

## Installation

You can install the released version of insiderTrades from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("insiderTrades")

```
Or you can install the most recent version of insiderTrades from [GitHub](https://github.com/US-Department-of-the-Treasury) with:

``` r
library(devtools)
devtools::install_github("US-Department-of-the-Treasury/insiderTrades")
```

In-depth vignettes can be found as part of the package. These vignettes walk the user through the fields of the returned dataframe object and the different types of functions available.

## Important Notes
There are two important columns to pay attention to in the generated dataframe, manyPeopleManyTransactions and Notes.

manyPeopleManyTransactions contains the URL if the filing had more than one rptOwner and multiple transactions. This is a warning indicator that the user must manually view this transaction and correctly assign the correct relationship between entities and transactions.

Possible values in Notes are the following:

* "The transaction values in this observation is an aggregate amount that is shared by the other observations that share the same URL."

This message means that there were many entities and one transaction in a filing. How this is reflected in the dataframe is that each entity has its own observation row but the transaction amounts are the same across each of these observation rows. The user should check the text file (through the associated URLs) and footnotes to make their own determination on how to structure a filing like this.

* "This transaction may not be a valid to key word conditions based upon the structure of many reporting owners. This transaction must be checked by hand."

This message means that there were many entities and many transactions in a filing. How it is reflected in the dataframe is that the information about the issuing company is included (periodOfReport, issuerCik, issuerName) and the information about the transaction and the associated footnotes is included. The user must first decide if the transaction is valid based upon their key word conditions if they used any rptOwnerKeywords key words and then second, determine which entity the transaction belongs to.
