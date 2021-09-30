#' @name transactionFilters
#' @rdname transactionFilters
#'
#' @title transactionFilters
#'
#' @param filing The object containing a single transaction from a Form 4
#' filing in text format
#' @param footnoteKeywords The character vector containing any specified key
#' words to be searched in the transaction's footnotes. Default is NA.
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

#' @rdname transactionFilters
#' @note \code{transactionFilterNonderivative} parses a transaction contained
#' in the filing and returns an integer value greater than 0 if one of the
#' transaction specific key word criteras or form specific key word criteras
#' is met.This function is specifically for the
#' \code{\link{nonderivativeTransactionsScrape}} and
#' \code{\link{nonderivativeTransactionsPullAndScrape}} functions.
transactionFilterNonderivative <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords, transactionType){

  # this section checks if the transaction contains any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<nonDerivativeTransaction>.*?(</nonDerivativeTransaction>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")
    footnoteTransactionMatches <- 0
    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified) | footnoteCitationIdentified == 0) {
        footnoteTransactionMatchesTemp <- 0
      }
      if (footnoteCitationIdentified > 0) {
        footnoteTransactionMatchesTemp <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
      }
      if (is.na(footnoteTransactionMatchesTemp )){
        footnoteTransactionMatchesTemp <- 0
      }
      footnoteTransactionMatches <- footnoteTransactionMatches + footnoteTransactionMatchesTemp
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerTransactionMatches <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerTransactionMatches )) {
      issuerTransactionMatches <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolMatches <-  filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolMatches)) {
      issuerTradingSymbolMatches <- 0
    }
  }

  # this section checks if the rptOnwer contains any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsMatches <- filing %>% stringr::str_extract(pattern = "<reportingOwner>.*?(</reportingOwner>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsMatches)) {
      rptOwnerKeywordsMatches <- 0
    }
  }

  # this section checks the transaction transactionType match the chosen transactionType(s)
  if (any(!is.na(transactionType))) {
    transactionTypeTemp <- sapply(transactionType, function(x) paste0('<transactionCode>',x,'</transactionCode>'))
    names(transactionTypeTemp) <- NULL
    transactionTypeMatches <- filing %>% stringr::str_extract(pattern = "<nonDerivativeTransaction>.*?(</nonDerivativeTransaction>)") %>% stringr::str_count(pattern = transactionTypeTemp) %>% sum()
    if (is.na(transactionTypeMatches)) {
      transactionTypeMatches <- 0
    }
  }

  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches", "transactionTypeMatches")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in the transaction
  keyTransactionCount <- sum(footnoteTransactionMatches, issuerTransactionMatches, issuerTradingSymbolMatches, rptOwnerKeywordsMatches, transactionTypeMatches)

  # return the sum of all the keywords identified in the transaction
  invisible(return(keyTransactionCount))
}

#' @rdname transactionFilters
#' @note \code{transactionFilterDerivative} parses a transaction contained
#' in the filing and returns an integer value greater than 0 if the
#' transaction contains one of the specific key word criteras or if the
#' key word criteria cannot be applied at the transaction level (
#' rptOwnerKeywords or issuerKeywords for example), but the form contains
#' form specific key word critera. This function is specifically for the
#' \code{\link{derivativeTransactionsScrape}} and
#' \code{\link{derivativeTransactionsPullAndScrape}} functions. The function
#' shares the same paramters as
#' \code{\link{formFilterNonderivativeTransactions}} (filing,
#' footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords,
#' transactionType). The only difference is within the parsing commands which
#' use the key word derivative rather than nonderivative.
transactionFilterDerivative <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords, transactionType){

  # this section checks if the transaction contains any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<derivativeTransaction>.*?(</derivativeTransaction>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")
    footnoteTransactionMatches <- 0
    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified) | footnoteCitationIdentified == 0) {
        footnoteTransactionMatchesTemp <- 0
      }
      if (footnoteCitationIdentified > 0) {
        footnoteTransactionMatchesTemp <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
      }
      if (is.na(footnoteTransactionMatchesTemp )){
        footnoteTransactionMatchesTemp <- 0
      }
      footnoteTransactionMatches <- footnoteTransactionMatches + footnoteTransactionMatchesTemp
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerTransactionMatches <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerTransactionMatches )) {
      issuerTransactionMatches <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolMatches <-  filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolMatches)) {
      issuerTradingSymbolMatches <- 0
    }
  }

  # this section checks if the rptOnwer contains any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsMatches <- filing %>% stringr::str_extract(pattern = "<reportingOwner>.*?(</reportingOwner>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsMatches)) {
      rptOwnerKeywordsMatches <- 0
    }
  }

  # this section checks the transaction transactionType match the chosen transactionType(s)
  if (any(!is.na(transactionType))) {
    transactionTypeTemp <- sapply(transactionType, function(x) paste0('<transactionCode>',x,'</transactionCode>'))
    names(transactionTypeTemp) <- NULL
    transactionTypeMatches <- filing %>% stringr::str_extract(pattern = "<derivativeTransaction>.*?(</derivativeTransaction>)") %>% stringr::str_count(pattern = transactionTypeTemp) %>% sum()
    if (is.na(transactionTypeMatches)) {
      transactionTypeMatches <- 0
    }
  }

  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches", "transactionTypeMatches")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in the transaction
  keyTransactionCount <- sum(footnoteTransactionMatches, issuerTransactionMatches, issuerTradingSymbolMatches, rptOwnerKeywordsMatches, transactionTypeMatches)

  # return the sum of all the keywords identified in the transaction
  invisible(return(keyTransactionCount))
}

#' @rdname transactionFilters
#' @note \code{transactionFilterNonderivativeHoldings} parses a transaction
#' contained in the filing and returns an integer value greater than 0 if the
#' transaction contains one of the specific key word criteras or if the
#' key word criteria cannot be applied at the transaction level (
#' rptOwnerKeywords or issuerKeywords for example), but the form contains
#' form specific key word critera. This function is specifically for the
#' \code{\link{nonderivativeHoldingsScrape}} and
#' \code{\link{nonderivativeHoldingsPullAndScrape}} functions. The function
#' shares the same paramters as
#' \code{\link{transactionFilterNonderivative}} (filing,
#' footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)
#' except for transactionType since that isn't a criteria for a nonderivative
#' holdings. Additionally, parsing criteria is different due to targeting
#' holding information rather than transaction information.
transactionFilterNonderivativeHoldings <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords){

  # this section checks if the transaction contains any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<nonDerivativeHolding>.*?(</nonDerivativeHolding>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")
    footnoteTransactionMatches <- 0
    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified) | footnoteCitationIdentified == 0) {
        footnoteTransactionMatchesTemp <- 0
      }
      if (footnoteCitationIdentified > 0) {
        footnoteTransactionMatchesTemp <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
      }
      if (is.na(footnoteTransactionMatchesTemp )){
        footnoteTransactionMatchesTemp <- 0
      }
      footnoteTransactionMatches <- footnoteTransactionMatches + footnoteTransactionMatchesTemp
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerTransactionMatches <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerTransactionMatches )) {
      issuerTransactionMatches <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolMatches <-  filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolMatches)) {
      issuerTradingSymbolMatches <- 0
    }
  }

  # this section checks if the rptOnwer contains any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsMatches <- filing %>% stringr::str_extract(pattern = "<reportingOwner>.*?(</reportingOwner>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsMatches)) {
      rptOwnerKeywordsMatches <- 0
    }
  }

  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in the transaction
  keyTransactionCount <- sum(footnoteTransactionMatches, issuerTransactionMatches, issuerTradingSymbolMatches, rptOwnerKeywordsMatches)

  # return the sum of all the keywords identified in the transaction
  invisible(return(keyTransactionCount))
}

#' @rdname transactionFilters
#' @note \code{transactionFilterDerivativeHoldings} parses a transaction
#' contained in the filing and returns an integer value greater than 0 if the
#' transaction contains one of the specific key word criteras or if the
#' key word criteria cannot be applied at the transaction level (
#' rptOwnerKeywords or issuerKeywords for example), but the form contains
#' form specific key word critera. This function is specifically for the
#' \code{\link{derivativeHoldingsScrape}} and
#' \code{\link{derivativeHoldingsPullAndScrape}} functions. The function
#' shares the same paramters as
#' \code{\link{transactionFilterNonderivative}} (filing,
#' footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords)
#' except for transactionType since that isn't a criteria for a derivative
#' holdings. The only difference is within the parsing commands which
#' use the key word derivative rather than nonderivative.
transactionFilterDerivativeHoldings <- function(filing, footnoteKeywords, issuerKeywords, issuerTradingSymbol, rptOwnerKeywords){

  # this section checks if the transaction contains any of the footnote keywords
  if (any(!is.na(footnoteKeywords))) {
    filingTransactionSection <- filing %>% stringr::str_extract(pattern = "<derivativeHolding>.*?(</derivativeHolding>)")
    filingFootnoteSection <- filing %>% stringr::str_extract(pattern = "<footnotes>.*?(</footnotes>)")
    footnoteTransactionMatches <- 0
    for (footnote in 1:30) {
      footnoteCitationIdentified <- stringr::str_count(filingTransactionSection, paste('footnoteId id="F',footnote,'"',sep=""))
      if (is.na(footnoteCitationIdentified) | footnoteCitationIdentified == 0) {
        footnoteTransactionMatchesTemp <- 0
      }
      if (footnoteCitationIdentified > 0) {
        footnoteTransactionMatchesTemp <- filingFootnoteSection %>% stringr::str_extract(pattern = paste0('<footnote id="F',footnote,'">.*?(</footnote>)')) %>% stringr::str_count(footnoteKeywords) %>% sum()
      }
      if (is.na(footnoteTransactionMatchesTemp )){
        footnoteTransactionMatchesTemp <- 0
      }
      footnoteTransactionMatches <- footnoteTransactionMatches + footnoteTransactionMatchesTemp
    }
  }

  # this section checks if the issuer section contains any of the keywords
  if (any(!is.na(issuerKeywords))) {
    issuerTransactionMatches <- filing %>% stringr::str_extract(pattern = "<issuer>.*?(</issuer>)") %>% stringr::str_count(issuerKeywords) %>% sum()
    if (is.na(issuerTransactionMatches )) {
      issuerTransactionMatches <- 0
    }
  }

  # this section checks if the issuer trading symbol section contains any of the chosen trading symbols
  if (any(!is.na(issuerTradingSymbol))) {
    issuerTradingSymbolMatches <-  filing %>% stringr::str_extract(pattern = "<issuerTradingSymbol>.*?(</issuerTradingSymbol>)") %>% stringr::str_count(issuerTradingSymbol) %>% sum()
    if (is.na(issuerTradingSymbolMatches)) {
      issuerTradingSymbolMatches <- 0
    }
  }

  # this section checks if the rptOnwer contains any of the keywords
  if (any(!is.na(rptOwnerKeywords))) {
    rptOwnerKeywordsMatches <- filing %>% stringr::str_extract(pattern = "<reportingOwner>.*?(</reportingOwner>)") %>% stringr::str_count(rptOwnerKeywords) %>% sum()
    if (is.na(rptOwnerKeywordsMatches)) {
      rptOwnerKeywordsMatches <- 0
    }
  }

  # If the keywrod value was NA, then the variable containing the count value is not created. If the count variable is missing, this loop creates the count variable with a value of 0
  exist <- c("footnoteTransactionMatches", "issuerTransactionMatches", "issuerTradingSymbolMatches", "rptOwnerKeywordsMatches")
  for (existing in exist) {
    if (exists(existing) == FALSE){
      assign(paste0(existing), 0)
    }
  }

  # sum up all the keywords identified in the transaction
  keyTransactionCount <- sum(footnoteTransactionMatches, issuerTransactionMatches, issuerTradingSymbolMatches, rptOwnerKeywordsMatches)

  # return the sum of all the keywords identified in the transaction
  invisible(return(keyTransactionCount))
}

#' @importFrom magrittr "%>%"
