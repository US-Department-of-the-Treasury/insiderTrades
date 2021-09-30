
delete.na <- function(df, n=0) {
  df[rowSums(is.na(df)) <= n,]
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
