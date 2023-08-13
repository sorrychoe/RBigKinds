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
#' top_words(df, "경향신문", top_n=30)
#' @import ggplot2
#' @import dplyr
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