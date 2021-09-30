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
#  tempIndex <- insiderTrades::secUrlDownload(quarter = 1, year = 2021, form = 4, name = "Your Name", "YourEmail@address.com")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  urlIndex <- tempIndex[1:100,]

## ---- eval=FALSE--------------------------------------------------------------
#  nonderivativeTrans <- insiderTrades::nonderivativeTransactionsScrape(index = urlIndex, form = 4, name = "Your Name", email = "YourEmail@YourEmail.com")

