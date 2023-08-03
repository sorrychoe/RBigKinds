keywords_wordcloud <- function(df, press) {
  if (is.data.frame(df)) {
    df_keywords <- df[df$언론사 == press, ]
    keywords <- keyword_list(df_keywords)
    news_key <- keyword_parser(keywords)
    news_key <- duplication_remover(news_key)
    key <- word_counter(news_key)
    news_key <- counter_to_dataframe(key)
    wc <- wordcloud::wordcloud(
      words = news_key$단어,
      freq = news_key$빈도,
      scale = c(3, 0.5),
      min.freq = 1,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8, "Dark2")
    )
    print(wc)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

