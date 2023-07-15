library(wordcloud)
library(ggplot2)

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

top_words <- function(df, press, top_n = 25) {
  if (is.data.frame(df)) {
    df_keywords <- df[grepl(press, df$언론사), ]
    keywords <- keyword_list(df_keywords)
    news_key <- keyword_parser(keywords)
    news_key <- duplication_remover(news_key)
    key <- word_counter(news_key)
    news_key <- counter_to_dataframe(key)
    data <- head(news_key[order(news_key$빈도, decreasing = TRUE), ], top_n)
    ggplot(data, aes(reorder(단어, -빈도), 빈도)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "단어", y = "빈도") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("언론사 별 사용 단어 빈도 상위", top_n) +
      coord_flip()
  } else {
    stop("input type is to be have to DataFrame")
  }
}

scatterplot <- function(df, label) {
  if (is.data.frame(df)) {
    ggplot(df, aes(component.0, component.1, color = label)) +
      geom_point() +
      labs(x = "component 0", y = "component 1") +
      ggtitle("Scatter plot for dimension reduction") +
      theme(legend.position = "bottom")
  } else {
    stop("input type is to be have to DataFrame")
  }
}
