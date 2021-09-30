#' @title secUrlDownload
#'
#' @description The function takes the specifieid quarter (1-4), year (2004-
#' 2021), and form type (4 or 5) and returns a dataframe of URLs to each form
#' filed during the specified quarter.
#'
#' @param quarter Specify the financial reporting quarter. Options are 1, 2, 3,
#' and 4.
#' @param year Specify the calendar year. Options range from 2004 to present.
#' @param form Specify the form type. Options are 4 or 5.
#' @param name Specify your name. This is required by the SEC.
#' @param email Specify your email. This is required by the SEC.
#' @return A dataframe consisting of the URLs to the Form 4 or 5 text filings
#' for the specified quarter and year.

#' @importFrom magrittr "%>%"
#' @importFrom utils "read.table"
#' @importFrom utils "download.file"


#' @export
secUrlDownload <- function(quarter, year, form, name, email){

  year <- as.character(year)
  form <- as.character(form)
  quarter <- as.character(quarter)

  # download and read in data from SEC
  file.create("dat.txt")
  datTxt <- paste0("dat.txt")
  download.file(url = paste("https://www.sec.gov/Archives/edgar/full-index/", year,"/","QTR", quarter,"/form.idx",sep=""), destfile = datTxt, method = "auto",
                quiet = TRUE, cacheOK = FALSE, headers = c("User-Agent" = paste(name, email, options("HTTPUserAgent"), sep = " ")))
  dat1 <- read.table(datTxt, skip = 10, header = FALSE, sep ='\t', quote = "", colClasses = c("character"), comment.char = "")

  # clean and rearrange
  dat1[,1] <- gsub("^SC ", "SC-", dat1[,1])

  dat2 <- as.data.frame(matrix(, ncol=3, nrow=nrow(dat1)))
  colnames(dat2) <- unlist(c("id", "formName", "filingUrl"))

  dat2[,1] <- seq(from=1, to=nrow(dat2), by = 1)
  dat2[,2] <- stringr::str_extract(dat1[,1], "^\\S*")
  dat2[,3] <- stringr::str_extract(dat1[,1], "edgar/.*")
  dat2[,3] <- paste("https://www.sec.gov/Archives/",dat2[,3], sep="")

  # filter dataset to contain only the forms interested in
  dat3 <- subset(dat2, dat2$formName == form)

  # remove scratch files and return final dataframe
  rm(datTxt, dat1, dat2)
  invisible(return(dat3))
}
