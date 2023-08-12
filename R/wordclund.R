keywords_wordcloud <- function(df, press) {
  if (is.data.frame(df)) {
    words <- keyword_dataframe(df)
    wordcloud2(words)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

