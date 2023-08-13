#' lda
#'
#' 토픽 모델링을 시행합니다.
#' 
#' @param df BigKinds 원본 문서
#' @param k 토픽 개수
#'
#' @examples
#' lda(df, k = 10)
#'
#' @import tidymodels
#' @import dplyr
#' @import tm
#' @export
lda <- function(dataframe, k = 10) {
  if (is.data.frame(dataframe)) {
    data <- word_tokenizer(dataframe)
    data <- data |> 
      count(키워드, 제목) |> 
      cast_dtm(키워드, 제목, n)
    model <- LDA(data, k = k)
    
    return(model)
  } else {
    stop("input type is to be have to DataFrame")
  }
}
