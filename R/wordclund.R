#' keywords_wordcloud
#'
#' 언론사 별로 가장 많이 등장한 단어 순위를 wordcloud로 시각화합니다.
#'
#' @param df BigKinds 원본 문서
#' @param press 확인할 언론사 이름
#'
#' @examples
#' keywords_wordcloud(df, "조선일보")
#' @import wordcloud2
#' @import dplyr
#' @export
keywords_wordcloud <- function(df, press=NA) {
  if (is.data.frame(df)) {
    if (!is.na(press)){
      df <- df |> filter(언론사 == press)
    }
    words <- keyword_dataframe(df)
    wordcloud2(words)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

