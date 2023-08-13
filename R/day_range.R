#' day_range
#'
#' 단어 범위를 return합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' day_range(df)
#'
#' @export
day_range <- function(df) {
  if (is.data.frame(df)) {
    print(paste("first day: ", min(df$일자)))
    print(paste("last day: ", max(df$일자)))
  } else {
    stop("input type is to be have to DataFrame")
  }
}