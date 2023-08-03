press_counter <- function(df) {
  if (is.data.frame(df)) {
    freq <- table(df$언론사)
    brod_df <- data.frame(언론사 = names(freq), 기사 = as.numeric(freq))
    return(brod_df)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

