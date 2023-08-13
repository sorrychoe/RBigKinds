#' press_counter
#'
#' 언론사 별 기사의 갯수를 반환합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' press_counter(df)
#' 
#' @export
press_counter <- function(df) {
  if (is.data.frame(df)) {
    freq <- table(df$언론사)
    brod_df <- data.frame(언론사 = names(freq), 기사 = as.numeric(freq))
    return(brod_df)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

