#' topic_modeling
#'
#' 토픽 모델링을 시행합니다.
#' 
#' @param df BigKinds 원본 문서
#' @param k 토픽 개수
#' @param method LDA 추정 방법 ("Gibbs" 또는 "VEM"), 기본값 "Gibbs"
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
#' topic_modeling(df, k = 10)
#'
#' @import topicmodels
#' @import dplyr
#' 
#' @export
topic_modeling <- function(df, k, method="Gibbs") {
  if (is.data.frame(df)) {
    data <- word_tokenizer(df)
    data <- data |>
      count(!!sym(.rb_col_keyword), !!sym(.rb_col_title)) |>
      tidytext::cast_dtm(!!sym(.rb_col_title), !!sym(.rb_col_keyword), n)
    model <- topicmodels::LDA(data, k = k, method = method)
    
    return(model)
  } else {
    stop("input type is to be have to dataframe")
  }
}