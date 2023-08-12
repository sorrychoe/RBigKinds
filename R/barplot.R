top_words <- function(df, press, top_n = 25) {
  if (is.data.frame(df)) {
    data <- keyword_dataframe(df)
    data <- head(data[order(data$n, decreasing = TRUE), ], top_n)
    ggplot(data, aes(reorder(키워드, n), n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "단어", y = "빈도") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("언론사 별 사용 단어 빈도 상위", top_n) +
      coord_flip()
  } else {
    stop("input type is to be have to DataFrame")
  }
}

