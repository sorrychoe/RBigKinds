#' association
#'
#' 기사에 등장한 단어 별로 연관분석을 진행합니다.
#' 연관분석 방법은 Apriori입니다.
#' 
#' @param df BigKinds 원본 문서
#' @param min_support 최소 지지도
#' @param minlen 연관된 최소 갯수
#' @param maxlen 연관된 최대 갯수
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
#' association(df, min_support = 0.1, minlen = 2, maxlen = 10)
#' 
#' @import arules
#' @import tm
#' 
#' @export
association <- function(df, min_support = 0.5, minlen=2, maxlen = 10) {
  if (is.data.frame(df)) {
    words <- word_tokenizer(df)
    data <- split(words$키워드, words$제목)
    te_data <- as(data, "transactions")
    result <- apriori(te_data, parameter = list(supp = min_support, minlen=minlen, maxlen=maxlen, target = "rules"))
    result <- as.data.frame(arules::inspect(result))
    result <- result[, c("lhs", "rhs", "support", "confidence")]
    colnames(result) <- c("lhs", "rhs", "support", "confidence")
    result <- result[result[, "confidence"] > min_support, ]
    return(result)
  } else {
    stop("input type is to be have to df")
  }
}