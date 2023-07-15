source(global.R)

keyword_dataframe <- function(df) {
  if (is.data.frame(df)) {
    lis <- keyword_list(df)
    keywords <- keyword_parser(lis)
    counter <- word_counter(keywords)
    df <- counter_to_dataframe(counter)
    return(df)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

keyword_dataframe_no_duplicated <- function(df) {
  if (is.data.frame(df)) {
    lis <- keyword_list(df)
    keywords <- keyword_parser(lis)
    keywords_set <- duplication_remover(keywords)
    counter <- word_counter(keywords_set)
    df <- counter_to_dataframe(counter)
    return(df)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

tfidf <- function(df, ...) {
  if (is.data.frame(df)) {
    if (length(...) > 0 && is.character(...)) {
      df <- df[, ...]
    }
    lis <- keyword_list(df)
    tfidfv <- DocumentTermMatrix(Corpus(VectorSource(lis)), control = list(weighting = weightTfIdf))
    word_count <- data.frame(
      단어 = colnames(tfidfv),
      빈도 = colSums(as.matrix(tfidfv))
    ) %>%
      arrange(desc(빈도)) %>%
      mutate(index = row_number()) %>%
      select(-index)
    return(word_count)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

tfidf_vector <- function(df) {
  if (is.data.frame(df)) {
    lis <- keyword_list(df)
    dtm <- DocumentTermMatrix(Corpus(VectorSource(lis)))
    tdm <- weightTfIdf(dtm)
    vec <- as.matrix(tdm)
    return(vec)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

normalize_vector <- function(vec) {
  if (is.matrix(vec)) {
    vec_nor <- t(normalize(t(vec)))
    return(vec_nor)
  } else {
    stop("input type is to be have to matrix")
  }
}
