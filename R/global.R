#' [] 표시된 헤더 삭제
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
header_remover <- function(df) {
  if (is.data.frame(df)) {
    ans <- gsub("\\[[^)]*\\]", "", df$`제목`)
    df$`제목` <- ans
    return(df)
  } else if (is.list(df)) {
    ans <- gsub("\\[[^)]*\\]", "", df)
    return(ans)
  } else {
    stop("input value is to be have to list or DataFrame")
  }
}

#' 키워드를 list로 변환
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
keyword_list <- function(df) {
  if (is.data.frame(df)) {
    return(df$`키워드`)
  } else if (is.list(df)) {
    return(df)
  } else {
    stop("input value is to be have to list or DataFrame")
  }
}

#' [] 키워드 파싱
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
keyword_parser <- function(text_list) {
  if (is.list(text_list)) {
    news_key <- list()
    for (word in text_list) {
      if (is.character(word)) {
        word <- strsplit(word, ",")[[1]]
        news_key <- c(news_key, list(word))
      } else {
        stop("input list is not valid format")
      }
    }
    return(news_key)
  } else {
    stop("input type is to be have to list")
  }
}

#' 중복 값 제거
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
duplication_remover <- function(news_key) {
  if (is.list(news_key)) {
    news_value <- list()
    for (j in news_key) {
      if (is.list(j)) {
        j <- unique(j)
        news_value <- c(news_value, list(j))
      } else {
        stop("input list is not valid format")
      }
    }
    return(news_value)
  } else {
    stop("input type is to be have to list")
  }
}

#' 단어 갯수 카운트
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
word_counter <- function(news_value) {
  if (is.list(news_value)) {
    key_words <- list()
    for (k in seq_along(news_value)) {
      for (i in news_value[[k]]) {
        if (!(i %in% names(key_words))) {
          key_words[[i]] <- 1
        } else {
          key_words[[i]] <- key_words[[i]] + 1
        }
      }
    }
    return(key_words)
  } else {
    stop("input type is to be have to list")
  }
}


#' counter dict를 dataframe으로 변환
#'
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
counter_to_dataframe <- function(key_words) {
  if (is.list(key_words)) {
    word_df <- data.frame(matrix(unlist(key_words), ncol = 2, byrow = TRUE))
    colnames(word_df) <- c("단어", "빈도")
    word_df <- word_df[order(word_df$`빈도`, decreasing = TRUE), , drop = FALSE, ]
    rownames(word_df) <- NULL
    return(word_df)
  } else {
    stop("input type is to be have to dict")
  }
}

#' df to Keyword_dataframe
#'
#'
#' @param df BigKinds 원본 문서
#' @return 키워드 데이터프레임으로 변환
#' @export
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

#' df to Keyword_dataframe of removed duplicated
#'
#'
#' @param df BigKinds 원본 문서
#' @return 키워드 데이터프레임으로 변환
#' @export
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

#' get tf-idf score
#'
#'
#' @param df BigKinds 원본 문서
#' @return tfidf 데이터프레임으로 변환
#' @export
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

#' change to tf-idf vector
#'
#'
#' @param df BigKinds 원본 문서
#' @return tfidf vector로 변환
#' @export
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

#' 벡터 정규화
#'
#'
#' @param df tfidf vector
#' @return 정규화 벡터로 전환
#' @export
normalize_vector <- function(vec) {
  if (is.matrix(vec)) {
    vec_nor <- t(normalize(t(vec)))
    return(vec_nor)
  } else {
    stop("input type is to be have to matrix")
  }
}
