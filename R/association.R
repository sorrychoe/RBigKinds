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
#' association(df, min_support = 0.6, minlen = 3, maxlen = 10)
#' @import arules
#' @export
association <- function(df, min_support = 0.5, minlen=2, maxlen = 10) {
  if (is.data.frame(df)) {
    words <- word_tokenizer(df)
    data <- split(words$키워드, words$제목)
    te_data <- as(data, "transactions")
    result <- apriori(te_data, parameter = list(supp = min_support, minlen=minlen, maxlen=maxlen, target = "rules"))
    result <- as.data.frame(inspect(result))
    result <- result[, c("lhs", "rhs", "support", "confidence")]
    colnames(result) <- c("lhs", "rhs", "support", "confidence")
    result <- result[result[, "confidence"] > min_support ]
    return(result)
  } else {
    stop("input type is to be have to df")
  }
}