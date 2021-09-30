#' @name formFilters
#' @rdname formFilters
#'
#' @title formFilters
#'
#' @param filing The object containing the Form 4 text
#' @param footnoteKeywords The character vector containing any specified key
#' words to be searched in the form's footnotes. Default is NA.
#' @param issuerKeywords The character vector containing any specified key
#' words to be searched within the issuer block. Default is NA.
#' @param issuerTradingSymbol The character vector containing any specified
#' stock tickers. Default is NA.
#' @param rptOwnerKeywords The character vector contianing any specified key
#' words to be searched within the rptOwner block. Default is NA.
#' @param transactionType The character vector containing any specified
#' transaction codes. Default is NA.
#' @return An integer value that if greater than 0, reflects that at the one of
#' the criteria parameters listed above has been met. The form will then be
#' parsed further. Otherwise, if the integer value is 0, the function pulls in
#' the next Form 4 to evaluate.
NULL

#' @rdname formFilters
#' @note \code{formFilterNonderivativeTransactions} parses the form and
#' returns an integer value greater than 0 if one of the key word
#' criteras is met. This function is specifically for the
#' \code{\link{nonderivativeTransactionsScrape}} and
#' \code{\link{nonderivativeTransactionsPullAndScrape}} functions.
formFilterNonderivativeTransactions <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords, transactionType){

  # this section checks if any transaction on the form contain any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    footnoteKeywordsCount <- 0
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<nonDerivativeTable>.*?(</nonDerivativeTable>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")

    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified)) {
        footnoteCitationIdentified <- 0
      }
      if (footnoteCitationIdentified > 0) {
        temp_value <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
        if (is.na(temp_value)){
          temp_value <- 0
        }
      } else {
        temp_value <- 0
      }

      footnoteKeywordsCount <- footnoteKeywordsCount + temp_value
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerKeywordsCount <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerKeywordsCount)) {
      issuerKeywordsCount <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolCount <- filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolCount)) {
      issuerTradingSymbolCount <- 0
    }
  }

  # this section checks if any of the rptOwners contain any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsCount <- filing %>% stringr::str_extract(pattern = "</issuer>.*?(<nonDerivativeTable>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsCount)){
      rptOwnerKeywordsCount <- 0
    }
  }

  # this section checks if any of the transaction transactionTypes match the chosen transactionTypes
  if (any(!is.na(transactionType))) {
    transactionTypeTemp <- sapply(transactionType, function(x) paste0('<transactionCode>',x,'</transactionCode>'))
    names(transactionTypeTemp) <- NULL
    transactionTypeCount <- filing %>% stringr::str_extract(pattern = "<nonDerivativeTable>.*?(</nonDerivativeTable>)") %>% stringr::str_count(pattern = transactionTypeTemp) %>% sum()
    if (is.na(transactionTypeCount)) {
      transactionTypeCount <- 0
    }
  }

  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteKeywordsCount", "issuerKeywordsCount", "issuerTradingSymbolCount", "rptOwnerKeywordsCount", "transactionTypeCount")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in a single form
  keyCount <- sum(footnoteKeywordsCount, issuerKeywordsCount, issuerTradingSymbolCount, rptOwnerKeywordsCount, transactionTypeCount)

  # return the sum of all the keywords identified in a single form
  invisible(return(keyCount))
}

#' @rdname formFilters
#' @note \code{formFilterDerivativeTransactions} parses the form and
#' returns an integer value greater than 0 if one of the key word criteras is
#' met. This function is specifically for the
#' \code{\link{derivativeTransactionsScrape}} and
#' \code{\link{derivativeTransactionsPullAndScrape}} functions. The function
#' shares the same paramters as
#' \code{\link{formFilterNonderivativeTransactions}} (filing,
#' footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords,
#' transactionType). The only difference is within the parsing commands which
#' use the key word derivative rather than nonderivative.
formFilterDerivativeTransactions <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords, transactionType){

  # this section checks if any transaction on the form contain any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    footnoteKeywordsCount <- 0
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<derivativeTable>.*?(</derivativeTable>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")

    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified)) {
        footnoteCitationIdentified <- 0
      }
      if (footnoteCitationIdentified > 0) {
        temp_value <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
        if (is.na(temp_value)){
          temp_value <- 0
        }
      } else {
        temp_value <- 0
      }

      footnoteKeywordsCount <- footnoteKeywordsCount + temp_value
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerKeywordsCount <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerKeywordsCount)) {
      issuerKeywordsCount <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolCount <- filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolCount)) {
      issuerTradingSymbolCount <- 0
    }
  }

  # this section checks if any of the rptOwners contain any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsCount <- filing %>% stringr::str_extract(pattern = "</issuer>.*?(<derivativeTable>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsCount)){
      rptOwnerKeywordsCount <- 0
    }
  }

  # this section checks if any of the transaction transactionTypes match the chosen transactionTypes
  if (any(!is.na(transactionType))) {
    transactionTypeTemp <- sapply(transactionType, function(x) paste0('<transactionCode>',x,'</transactionCode>'))
    names(transactionTypeTemp) <- NULL
    transactionTypeCount <- filing %>% stringr::str_extract(pattern = "<derivativeTable>.*?(</derivativeTable>)") %>% stringr::str_count(pattern = transactionTypeTemp) %>% sum()
    if (is.na(transactionTypeCount)) {
      transactionTypeCount <- 0
    }
  }

  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteKeywordsCount", "issuerKeywordsCount", "issuerTradingSymbolCount", "rptOwnerKeywordsCount", "transactionTypeCount")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in a single form
  keyCount <- sum(footnoteKeywordsCount, issuerKeywordsCount, issuerTradingSymbolCount, rptOwnerKeywordsCount, transactionTypeCount)

  # return the sum of all the keywords identified in a single form
  invisible(return(keyCount))
}

#' @rdname formFilters
#' @note \code{formFilterNonderivativeHoldings} parses the form and
#' returns an integer value greater than 0 if one of the key word criteras is
#' met. This function is specifically for the
#' \code{\link{nonderivativeHoldingsScrape}} and
#' \code{\link{nonderivativeHoldingsPullAndScrape}} functions. The function
#' shares the same paramters as
#' \code{\link{formFilterNonderivativeTransactions}} (filing,
#' footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)
#' except for transactionType since that isn't a criteria for a nonderivative
#' holdings. Additionally, parsing criteria is different due to targeting
#' holding information rather than transaction information.
formFilterNonderivativeHoldings <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords){

  # this section checks if any transaction on the form contain any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    footnoteKeywordsCount <- 0
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<nonDerivativeTable>.*?(</nonDerivativeTable>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")

    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified)) {
        footnoteCitationIdentified <- 0
      }
      if (footnoteCitationIdentified > 0) {
        temp_value <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
        if (is.na(temp_value)){
          temp_value <- 0
        }
      } else {
        temp_value <- 0
      }

      footnoteKeywordsCount <- footnoteKeywordsCount + temp_value
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerKeywordsCount <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerKeywordsCount)) {
      issuerKeywordsCount <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolCount <- filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolCount)) {
      issuerTradingSymbolCount <- 0
    }
  }

  # this section checks if any of the rptOwners contain any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsCount <- filing %>% stringr::str_extract(pattern = "</issuer>.*?(<nonDerivativeTable>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsCount)){
      rptOwnerKeywordsCount <- 0
    }
  }


  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteKeywordsCount", "issuerKeywordsCount", "issuerTradingSymbolCount", "rptOwnerKeywordsCount")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in a single form
  keyCount <- sum(footnoteKeywordsCount, issuerKeywordsCount, issuerTradingSymbolCount, rptOwnerKeywordsCount)

  # return the sum of all the keywords identified in a single form
  invisible(return(keyCount))
}

#' @rdname formFilters
#' @note \code{formFilterDerivativeHoldings} parses the form and
#' returns an integer value greater than 0 if one of the key word criteras is
#' met. This function is specifically for the
#' \code{\link{derivativeHoldingsScrape}} and
#' \code{\link{derivativeHoldingsPullAndScrape}} functions. The function
#' shares the same paramters as
#' \code{\link{formFilterNonderivativeHoldings}} (filing,
#' footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)
#' except for transactionType since that isn't a criteria for a derivative
#' holdings. The only difference is within the parsing commands which
#' use the key word derivative rather than nonderivative.
formFilterDerivativeHoldings <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords){

  # this section checks if any transaction on the form contain any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    footnoteKeywordsCount <- 0
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<derivativeTable>.*?(</derivativeTable>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")

    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified)) {
        footnoteCitationIdentified <- 0
      }
      if (footnoteCitationIdentified > 0) {
        temp_value <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
        if (is.na(temp_value)){
          temp_value <- 0
        }
      } else {
        temp_value <- 0
      }

      footnoteKeywordsCount <- footnoteKeywordsCount + temp_value
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerKeywordsCount <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerKeywordsCount)) {
      issuerKeywordsCount <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolCount <- filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolCount)) {
      issuerTradingSymbolCount <- 0
    }
  }

  # this section checks if any of the rptOwners contain any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsCount <- filing %>% stringr::str_extract(pattern = "</issuer>.*?(<derivativeTable>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsCount)){
      rptOwnerKeywordsCount <- 0
    }
  }


  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteKeywordsCount", "issuerKeywordsCount", "issuerTradingSymbolCount", "rptOwnerKeywordsCount")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in a single form
  keyCount <- sum(footnoteKeywordsCount, issuerKeywordsCount, issuerTradingSymbolCount, rptOwnerKeywordsCount)

  # return the sum of all the keywords identified in a single form
  invisible(return(keyCount))
}

#' @importFrom magrittr "%>%"
