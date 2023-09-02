#' top_words
#'
#' 언론사 별로 가장 많이 등장한 단어 순위를 시각화합니다.
#' 최대 몇개의 단어를 추출할지는 직접 정할 수 있습니다.
#' default는 25개입니다.
#' 
#' @param df BigKinds 원본 문서
#' @param press 확인할 언론사 이름
#' @param top_n 시각화할 단어 갯수
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
#' top_words(df, "경향신문", top_n=10)
#' 
#' @import ggplot2
#' @import dplyr
#' 
#' @export
top_words <- function(df, press = NA, top_n = 25) {
  if (is.data.frame(df)) {
    if (!is.na(press)){
      df <- df |> filter(언론사 == press)
    }
    data <- keyword_dataframe(df)
    data <- head(data[order(data$n, decreasing = TRUE), ], top_n)
    ggplot(data, aes(reorder(키워드, n), n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(x = "단어", y = "빈도") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle("사용 단어 빈도 상위", top_n) +
      coord_flip()
  } else {
    stop("input type is to be have to DataFrame")
  }
}