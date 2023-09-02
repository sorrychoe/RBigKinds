#' header_remover
#'
#' 상단에 존재하는 헤더를 제거합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#'                 
#' data <- header_remover(df)
#' head(data)
#' 
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

#' word_tokenizer
#'
#' 파일로부터 문서 별 키워드로 나열된 데이터 프레임으로 변환합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#' data <- word_tokenizer(df)
#' head(data)
#' 
#' @import tm
#' @import tibble
#' @import dplyr
#' 
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


#' keyword_dataframe
#'
#' BigKinds 데이터 셋을 키워드 갯수 데이터프레임으로 변환합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#'                 
#' data <- keyword_dataframe(df)
#' head(data)
#' 
#' @import tm
#' @import tibble
#' @import dplyr
#' 
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

#' keyword_dataframe_no_duplicated
#'
#' BigKinds 데이터 셋을 키워드 갯수 데이터프레임(중복 미포함)으로 변환합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#'                 
#' data <- keyword_dataframe_no_duplicated(df)
#' head(data)
#' 
#' @import tm
#' @import tibble
#' @import dplyr
#' 
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

#' tfidf
#'
#' 키워드의 tfidf score를 포함한 데이터 프레임을 반환합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#'                 
#' data <- tfidf(df)
#' head(data)
#' 
#' @import tm
#' @import tibble
#' @import dplyr
#' @import tidytext
#' 
#' @export
tfidf <- function(df) {
  if (is.data.frame(df)) {
    data <- word_tokenizer(df) 
    data |>
      bind_tf_idf(term = `키워드`, document = `제목`, n = rowid) -> tfidf
    return(tfidf)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

#' tfidf_vector
#'
#' tfidf vector로 변환합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#'                 
#' data <- tfidf_vector(df)
#' head(data)
#' 
#' @import tm
#' @import tibble
#' @import dplyr
#' @import tidytext
#' 
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

#' normalize_vector
#'
#' 벡터를 정규화합니다.(row 기준 minmax scaling)
#'
#' @param vec tfidf vector
#' 
#' @examples
#' df <- data.frame(
#'   "일자" = c(20230101, 20230102, 20230103),
#'   "언론사" = c("조선일보", "한겨례", "경향신문"),
#'   "제목" = c("[속보] 경찰, 민주노총 도심 집회 1차 해산명령", 
#'               "[사설] 한반도 긴장 높인 북한의 군사정찰위성 발사 규탄한다",
#'               "‘직권남용죄’ 남용 유감"),
#'   "키워드" = c("경찰,해산명령,민주,노총,도심,집회,해산,명령,민주노총",
#'                 "한반도,긴장,북한,규탄,군사,정찰,위성,발사,북한,인공위성",
#'                 "직권남용죄,남용,유감,왜냐면,이정환,민주사회,변호사모임"))
#'                 
#' data <- tfidf_vector(df)
#' nor_data <- normalize_vector(data)
#' head(data)
#'
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
