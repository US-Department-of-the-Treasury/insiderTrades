#' @title derivativeHoldingsScrape
#'
#' @description The function takes the dataframe returned by
#' \code{\link{secUrlDownload}} and scrapes and compiles a dataframe of all
#' the derivative holdings that meet the keyword crtiera. If no criteria
#' is set, all holdings from the dataframe returned by
#' \code{\link{secUrlDownload}} are scraped. Note that the logic for the
#' keywords is "OR." Thus if a user sets multiple footnoteKeywords and a
#' keyword for issuerKeywords, if a holding has only one of the
#' footnoteKeywords and none of the issuerKeywords, the holding will
#' still be scraped.
#'
#' @param index A local object that was returned by utilizing the
#' \code{\link{secUrlDownload}}. It contains the URLs necessary to grab each
#' text version of the filed Form 4 or 5.
#' @param form Define if the form is a Form 4 or Form 5. Must match the type
#' used in \code{\link{secUrlDownload}}.
#' @param name Specify your name. This is required by the SEC.
#' @param email Specify your email. This is required by the SEC.
#' @param footnoteKeywords The character vector containing any specified key
#' words to be searched in the form's footnotes. Default is NA.
#' @param issuerKeywords The character vector containing any specified key
#' words to be searched within the issuer block. Criteria can include the firms
#' CIK or name. Values must be captilized to match against SEC values.
#' Default value is NA.
#' @param issuerTradingSymbol The character vector containing any specified
#' stock tickers.  Values must be captilized to match against SEC values.
#' Default value is NA.
#' @param rptOwnerKeywords The character vector contianing any specified key
#' words to be searched within the rptOwner block.  Values must be captilized
#' to match against SEC values. Additionally, the format is LAST NAME, FIRST
#' NAME. The criteria can include the individuals CIK, address, or name.
#' Default value is NA.
#' @return A dataframe containing containing the scraped information where
#' each row represents a derivative holding The holding observations
#' can be grouped by form through the URL variable within the dataframe.

#' @importFrom magrittr "%>%"
#' @importFrom utils "read.table"

#' @export
derivativeHoldingsScrape <- function(index, form, name, email, footnoteKeywords = NA, issuerKeywords = NA, issuerTradingSymbol = NA, rptOwnerKeywords = NA) {

  # core functions
  transactionScrape <- function(filing, counter, dat, n, k, columnNumber) {

    # Loop through the column names related to the transaction info the scraper collects
    for (columnName in columnNames[n:k]) {
      # create the three filters that are used to bracket and extract the desired information
      filterOne <- paste(a1,columnName,a2,columnName,a3,sep="")
      filterTwo <- paste(e1,columnName,e2,sep="")
      filterThree <- paste(e3,columnName,e2,sep="")

      # extract the information and save it in the appropriate column
      valueOne = filing %>% stringr::str_extract(pattern = (filterOne))
      valueTwo <- gsub(filterTwo, '', valueOne)
      valueThree <- gsub(filterThree, '', valueTwo)
      dat[counter, columnNumber] <- valueThree

      columnNumber <- columnNumber + 1
    }
    return(dat)
  }

  footnoteScrape <- function(filing, counter, dat, n, k, footnoteNumber, columnNumber) {
    transactionSection <- filing %>% stringr::str_extract(pattern = "<derivativeHolding>.*?(</derivativeHolding>)")
    footnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")

    # Loop the footnote column names and extracts the information related to the transaction
    for (columnName in columnNames[n:k]) {
      # checks if transaction has footnotes
      countTotal <- stringr::str_count(transactionSection, paste('footnoteId id="F',footnoteNumber,'"',sep=""))

      # if the transaction has footnotes, it extracts the footnote and saves it in the appropriate column
      if (countTotal > 0) {
        filterOne <- paste(a1, paste('footnote id="F',footnoteNumber,'"',sep=""), a2,'footnote',a3,sep="")
        filterTwo <- paste(e1,paste('footnote id="F',footnoteNumber,'"',sep=""),e2,sep="")
        filterThree <- paste(e3,'footnote',e2,sep="")

        valueOne <- footnoteSection %>% stringr::str_extract(pattern = (filterOne))
        valueTwo <- gsub(filterTwo, '', valueOne)
        valueThree <- gsub(filterThree, '', valueTwo)
        dat[counter, columnNumber] <- valueThree
      }

      columnNumber <- columnNumber + 1
      footnoteNumber <- footnoteNumber + 1

    }
    invisible(return(dat))
  }

  # format inputs to the function
  form <- as.character(form)

  # create empty dataframe and set column names. It is more efficient to create the matrix and substitute in values than to append. If there are no keywords, we expect at maximum
  # there will be six transactions for each filing.
  dat <- as.data.frame(matrix(, ncol=54, nrow=nrow(index)*6))
  columnNames <- c("periodOfReport", "issuerCik", "issuerName", "rptOwnerCik", "rptOwnerName", "rptOwnerState", "rptOwnerZipCode", "isDirector", "isOfficer", "isTenPercentOwner", "isOther", "officerTitle", "securityTitle", "conversionOrExercisePrice", "exerciseDate", "expirationDate", "underlyingSecurityTitle", "underlyingSecurityShares", "sharesOwnedFollowingTransaction", "directOrIndirectOwnership", "natureOfOwnership", "footnote1", "footnote2", "footnote3", "footnote4", "footnote5", "footnote6", "footnote7", "footnote8", "footnote9", "footnote10", "footnote11", "footnote12", "footnote13", "footnote14", "footnote15", "footnote16", "footnote17", "footnote18", "footnote19", "footnote20", "footnote21", "footnote22", "footnote23", "footnote24", "footnote25", "footnote26", "footnote27", "footnote28", "footnote29", "footnote30", "URL", "manyPeopleManyTransactions", "Notes")
  colnames(dat) <- columnNames

  # create .txt file to use as the sandbox for the string manipulations
  prefiling <- paste0("prefiling.txt")

  # global filter variables
  a1 <- '<'
  a2 <- '>.*?(</'
  a3 <- '>)'
  e1 <- '<'
  e2 <- '>'
  e3 <- '</'

  # loop that takes each url, pulls down the filing, checks for key words (or lack there of), and pulls the information into a column/row format if the keyword conditions are satisfied
  counter <- 0
  statusCounter <- 0
  totalRecords <- nrow(index) # minimize computation

  for (i in index[,3]) {
    filing_error <- try(download.file(i, prefiling, method = "auto", quiet = TRUE, cacheOK = FALSE,
                                      headers = c("User-Agent" = paste(name, email, options("HTTPUserAgent"), sep = " "))))
    # if an error occurs, it is likely because the SEC has blocked the IP address. The scraper waits 15 minutes before sending another request.
    if (class(filing_error) == "try-error") {
      Sys.sleep(900)
      filing_error <- try(download.file(i, prefiling, method = "auto", quiet = TRUE, cacheOK = FALSE,
                                        headers = c("User-Agent" = paste(name, email, options("HTTPUserAgent"), sep = " "))))
    }

    # Track progress through the index of filings
    statusCounter <- statusCounter + 1
    if (statusCounter %% 100 == 0){
      print(paste0(round((statusCounter/totalRecords)*100, digits = 2),"% Complete"))
    }

    # Limit program to 3 to 4 requests to the SEC server every second
    Sys.sleep(.05)

    # These following lines clean the document of uncessary characters and limits us to just the non-derivitive section
    filing <- readLines(prefiling) %>%
      stringr::str_c(collapse = " ") %>%
      stringr::str_extract(pattern = paste("(?s)(?m)<TYPE>",form,".*?(</TEXT>)",sep="")) %>%
      stringr::str_replace(pattern = "((?i)<TYPE>).*?(?=<)", replacement = "") %>%
      stringr::str_replace(pattern = "((?i)<SEQUENCE>).*?(?=<)", replacement = "") %>%
      stringr::str_replace(pattern = "((?i)<FILENAME>).*?(?=<)", replacement = "") %>%
      stringr::str_replace(pattern = "((?i)<DESCRIPTION>).*?(?=<)", replacement = "") %>%
      stringr::str_replace(pattern = "(?s)(?i)<head>.*?</head>", replacement = "") %>%
      stringr::str_replace(pattern = "(?s)(?i)<(table).*?(</table>)", replacement = "") %>%
      stringr::str_replace(pattern = "(?s)(?i)<(nonDerivativeTable).*?(</nonDerivativeTable>)", replacement = "") %>%
      stringr::str_replace_all(pattern = "&(.{2,6});", replacement = " ") %>%
      stringr::str_replace_all(pattern = "(?s) +", replacement = " ") %>%
      stringr::str_replace_all(pattern = "<value>", replacement = "") %>%
      stringr::str_replace_all(pattern = "</value>", replacement = "") %>%
      stringr::str_replace_all(pattern = "<derivativeTransaction>.*?(</derivativeTransaction>)", replacement = "") %>%
      stringr::str_replace_all(pattern = "<nonDerivativeHolding>.*?(</nonDerivativeHolding>)", replacement = "")

    # the function is the first filter. It counts of the number of keywords a form has. If it is greater than zero, it moves onto the switch which determines how many reporting owners
    # it has and how many transactions. Once that has been determined, the form goes through a second filter at the individual transaction level to ensure that only transactions that
    # meet the keyword criteria are scraped. By having this first filter, it cuts down on computation (thus resources and time)

    # now known as formFilterDerivativeHolding
    keyCount <- formFilterDerivativeHoldings(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)

    if (keyCount > 0 | (any(is.na(footnoteKeywords)) & any(is.na(issuerKeywords)) & any(is.na(issuerTradingSymbol)) & any(is.na(rptOwnerKeywords)))) {

      # This determines the amount of people in the filing and the number of filings in the record. This then dictates which loop will clean the document
      filingCount <- stringr::str_count(filing, "derivativeHolding> <securityTitle>")
      peopleCount <- stringr::str_count(filing, "<reportingOwner>")

      # One person/entity and one transaction loop
      if (filingCount == 1 & peopleCount == 1) {

        # Second filter at the transaction level checking to see if the transaction satisfies the keyword criteria. If it does, the information is scraped. If it doesn't, a new filing is pulled
        keyTransactionCount <- transactionFilterDerivativeHoldings(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)

        if (keyTransactionCount > 0 | (any(is.na(footnoteKeywords)) & any(is.na(issuerKeywords)) & any(is.na(issuerTradingSymbol)) & any(is.na(rptOwnerKeywords)))) {

          counter = counter + 1

          # This section scrapes the information for the columns associated with transaction details. Column index 53 and 54 remain NA since we are able to scrape these transactions
          dat <- transactionScrape(filing = filing, counter = counter, dat = dat, n = 1, k = 21, columnNumber = 1)

          # This section identifies if there are any footnotes associated with the transaction and if so, scrapes the information and deposists it in the correct column
          dat <- footnoteScrape(filing = filing, counter = counter, dat = dat, n = 22, k = 51, footnoteNumber = 1, columnNumber = 22)

          # Insert the URL
          dat[counter, 52] <- i

        }
        # Removes the count variables associated with each keyword type before running the second filter for the next transaction
        exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches")
        for (existing in exist) {
          if (exists(existing) == TRUE){
            rm(existing)
          }
        }

        # One person/entity but multiple transactions loop
      } else if (filingCount > 1 & peopleCount == 1) {

        transactionCount <- stringr::str_count(filing, "derivativeHolding> <securityTitle>")

        # by deleting the transaction when we are done with it from the .txt document, this allows us to sequentially move through each transaction in the filing. When transactionCount == 0,
        # the loop then knows to move onto pulling the next form.
        while (transactionCount >= 1){

          # Second filter at the transaction level checking to see if the transaction satisfies the keyword criteria. If it does, the information is scraped. If it doesn't, a new filing is pulled
          keyTransactionCount <- transactionFilterDerivativeHoldings(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)

          if (keyTransactionCount > 0 | (any(is.na(footnoteKeywords)) & any(is.na(issuerKeywords)) & any(is.na(issuerTradingSymbol)) & any(is.na(rptOwnerKeywords)))) {

            filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<derivativeHolding>.*?(</derivativeHolding>)")

            counter = counter + 1

            #This section scrapes the information for the columns associated with transaction details. Column index 53 and 54 remain NA since we are able to scrape these transactions
            dat <- transactionScrape(filing = filing, counter = counter, dat = dat, n = 1, k = 21, columnNumber = 1)

            # This section identifies if there are any footnotes associated with the transaction and if so, scrapes the information and deposists it in the correct column
            dat <- footnoteScrape(filing = filing, counter = counter, dat = dat, n = 22, k = 51, footnoteNumber = 1, columnNumber = 22)

            # Insert the URL
            dat[counter, 52] <- i

          }

          # Remove the transaction that was just reviewed and recalculate how many transaction remain
          filing <- filing %>% stringr::str_replace(pattern = "<derivativeHolding>.*?(</derivativeHolding>)", replacement = "")
          transactionCount <- stringr::str_count(filing, "derivativeHolding> <securityTitle>")

          # Removes the count variables associated with each keyword type before running the second filter for the next transaction
          exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches")
          for (existing in exist) {
            if (exists(existing) == TRUE){
              rm(existing)
            }
          }
        }

        # Multiple people/entities and one transaction loop
      } else if (filingCount == 1 & peopleCount > 1) {

        peopleCountTwo <- stringr::str_count(filing, "<reportingOwner>")

        # by deleting each rptOwner when we are done with it from the .txt document, this allows us to sequentially move through each rptOwner in the filing. When peopleCountTwo == 0,
        # the loop then knows to move onto pulling the next form.
        while (peopleCountTwo >= 1) {

          # Second filter at the transaction level checking to see if the transaction satisfies the keyword criteria. If it does, the information is scraped. If it doesn't, a new filing is pulled
          keyTransactionCount <- transactionFilterDerivativeHoldings(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)

          if (keyTransactionCount > 0 | (any(is.na(footnoteKeywords)) & any(is.na(issuerKeywords)) & any(is.na(issuerTradingSymbol)) & any(is.na(rptOwnerKeywords)))) {
            counter = counter + 1

            # This section scrapes the information for the columns associated with transaction details. Column index 53 remains NA since we are able to scrape these transactions
            dat <- transactionScrape(filing = filing, counter = counter, dat = dat, n = 1, k = 21, columnNumber = 1)

            # This section identifies if there are any footnotes associated with the rptOnwer or the transaction if so, scrapes the information and deposists it in the correct column

            # this section is for the reporting individuals
            individualFootnotes <- filing %>% stringr::str_extract(pattern = "<reportingOwner>.*?(</reportingOwner>)")

            columnCountTwo <- 22
            footnoteCount <- 1

            for (columnName in columnNames[22:51]) {
              countTotal <- stringr::str_count(individualFootnotes, paste('footnoteId id="F',footnoteCount,'"',sep=""))

              if (countTotal > 0) {
                filterOne <- paste(a1, paste('footnote id="F',footnoteCount,'"',sep=""), a2,'footnote',a3,sep="")
                filterTwo <- paste(e1,paste('footnote id="F',footnoteCount,'"',sep=""),e2,sep="")
                filterThree <- paste(e3,'footnote',e2,sep="")

                valueOne <- filing %>% stringr::str_extract(pattern = (filterOne))
                valueTwo <- gsub(filterTwo, '', valueOne)
                valueThree <- gsub(filterThree, '', valueTwo)
                dat[counter, columnCountTwo] <- valueThree
              }

              columnCountTwo <- columnCountTwo + 1
              footnoteCount <- footnoteCount + 1

            }

            # this section is for the transaction
            filingFootnotes <- filing %>% stringr::str_extract(pattern = "<derivativeHolding>.*?(</derivativeHolding>)")

            dat <- footnoteScrape(filing = filing, counter = counter, dat = dat, n = 22, k = 51, footnoteNumber = 1, columnNumber = 51)

            # Insert the URL
            dat[counter, 52] <- i

            # Make a note what type of transaction this was
            dat[counter, 54] <- "The transaction values in this observation is an aggregate amount that is shared by the other observations that share the same URL"
          }

          # Remove the rptOwner that was just reviewed and recalculate how many rptOwners remain
          filing <- filing %>% stringr::str_replace(pattern = "<reportingOwner>.*?(</reportingOwner>)", replacement = "")
          peopleCountTwo <- stringr::str_count(filing, "<reportingOwner>")

          # Removes the count variables associated with each keyword type before running the second filter for the next transaction
          exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches")
          for (existing in exist) {
            if (exists(existing) == TRUE){
              rm(existing)
            }
          }
        }
        # Many persons/entities and many transactions loop
      } else if (filingCount > 1 & peopleCount > 1) {
        transactionCount <- stringr::str_count(filing, "derivativeHolding> <securityTitle>")

        # By deleting the transaction when we are done with it from the .txt document, this allows us to sequentially move through each transaction in the filing. When transactionCount == 0,
        # the loop then knows to move onto pulling the next form.
        # This scrapes all the transaction information. The only items people will need to do by hand is going by hand and assigning the right individual
        while (transactionCount >= 1){

          # Second filter at the transaction level checking to see if the transaction satisfies the keyword criteria. If it does, the information is scraped. If it doesn't, a new filing is pulled
          keyTransactionCount <- transactionFilterDerivativeHoldings(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)

          if (keyTransactionCount > 0 | (any(is.na(footnoteKeywords)) & any(is.na(issuerKeywords)) & any(is.na(issuerTradingSymbol)) & any(is.na(rptOwnerKeywords)))) {

            counter = counter + 1

            # This section scrapes the information for the columns associated with transaction details. Columns 4 through 12 are left empty for the individual to hand code after looking
            # at the transaction
            dat <- transactionScrape(filing = filing, counter = counter, dat = dat, n = 1, k = 3, columnNumber = 1)

            dat <- transactionScrape(filing = filing, counter = counter, dat = dat, n = 13, k = 21, columnNumber = 13)

            # This section identifies if there are any footnotes associated with the transaction and if so, scrapes the information and deposists it in the correct column

            # pulls the block of text for the filing we are currently scraping
            filingFootnotes <- filing %>% stringr::str_extract(pattern = "<nonDerivativeHolding>.*?(</nonDerivativeHolding>)")

            dat <- footnoteScrape(filing = filing, counter = counter, dat = dat, n = 21, k = 51, footnoteNumber = 1, columnNumber = 21)

            # Insert the URL
            dat[counter, 53] <- i
            # Insert note about the transaction type
            dat[counter, 54] <- "This transaction may not be a valid to key word conditions based upon the structure of many reporting owners. This transaction must be checked by hand."

          }

          # Remove the transaction that was just reviewed and recalculate how many transaction remain
          filing <- filing %>% stringr::str_replace(pattern = "<derivativeHolding>.*?(</derivativeHolding>)", replacement = "")
          transactionCount <- stringr::str_count(filing, "derivativeHolding> <securityTitle>")

          # Removes the count variables associated with each keyword type before running the second filter for the next transaction
          exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches")
          for (existing in exist) {
            if (exists(existing) == TRUE){
              rm(existing)
            }
          }
        }
      }
    }
  }

  # remove empty rows in the final dataset. We initially create a dataset with the 6 times the number of rows as there are filings. But if keywords are used, we will end up with fewer rows
  # filled and this removes any empty rows.
  dat2 <- delete.na(dat, ncol(dat) - 1)

  # remove footnote citations within columns with variables
  dat2[,4:21] <- lapply(dat2[,4:21], function(x) { as.character(gsub('<footnoteId id="F([1-9]|[1-9][0-9])"/>', "", x))})

  invisible(return(dat2))
}
