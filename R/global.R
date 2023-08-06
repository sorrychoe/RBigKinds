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
  } else {
    stop("input value is to be have to list or DataFrame")
  }
}

#' tokenizer
#'
#' @param df BigKinds 원본 문서
#' @return 키워드 데이터프레임으로 변환
#' @export
word_tokenizer <- function(df) {
  if (is.data.frame(df)) {
    df |> 
      select(`제목`,`키워드`) |> 
      rowid_to_column() |> 
      unnest_tokens(
        input = "키워드",
        output = "키워드"
      ) -> keywords
    return(keywords)
  } else {
    stop("input value is to be have to list or DataFrame")
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
    data <- word_tokenizer(df) 
    data |> 
      group_by(키워드) |> 
      tally() |> 
      arrange(desc(n)) |> 
      as_tibble() -> keywords
    return(keywords)
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
    data <- word_tokenizer(df) 
    
    keywords_no_duplicated <- data[!duplicated(data[,c(2,3)]),]
    
    keywords_no_duplicated |> 
      group_by(키워드) |> 
      tally() |> 
      arrange(desc(n)) |> 
      as_tibble() -> return_keywords
    return(return_keywords)
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
    data <- word_tokenizer(df) 
    data |>
      bind_tf_idf(term = `키워드`, document = `제목`, n = rowid) -> tfidf
    return(tfidf)
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
    data <- word_tokenizer(df) 
    
    dtm <- DocumentTermMatrix(Corpus(VectorSource(data$키워드)))
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
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
