#' topic_modeling
#'
#' 토픽 모델링을 시행합니다.
#' 
#' @param df BigKinds 원본 문서
#' @param k 토픽 개수
#'
#' @examples
#' topic_modeling(df, k = 10)
#'
#' @import topicmodels
#' @import dplyr
#' @import tm
#' @export
topic_modeling <- function(dataframe, k, method="Gibbs") {
  if (is.data.frame(dataframe)) {
    data <- word_tokenizer(dataframe)
    data <- data |> 
      count(키워드, 제목) |> 
      tidytext::cast_dtm(제목, 키워드, n)
    model <- topicmodels::LDA(data, k = k, method = method)
    
    return(model)
  } else {
    stop("input type is to be have to DataFrame")
  }
}