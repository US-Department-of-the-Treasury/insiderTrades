## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include=FALSE-----------------------------------------------------------
devtools::load_all(".")

## ----install, eval=FALSE------------------------------------------------------
#  install.packages("insiderTrades")

## ----install2, eval=FALSE-----------------------------------------------------
#  library(devtools)
#  devtools::install_github("US-Department-of-the-Treasury/insiderTrades")

## ---- eval=FALSE--------------------------------------------------------------
#  library(insiderTrades)
#  
#  dat1 <- insiderTrades::nonderivativeHoldingsPullAndScrape(quarter = 1, year = 2021, form = 4, name = "Your Name", email = "YourEmail@YourEmail.com", footnoteKeywords = c("gift", "charity", "charitable"), transactionType = "G", rptOwnerKeywords = "SMITH")
#  
#  
#  dat2 <- insiderTrades::nonderivativeHoldingsPullAndScrape(quarter = 1, year = 2021, form = 4, name = "Your Name", email = "YourEmail@YourEmail.com", footnoteKeywords = c("gift", "charity", "charitable"))
#  

