#' Document Co-occurrence Matrix
#'
#' 단어 동시 출현 행렬을 형성합니다.
#' 
#' @param df BigKinds 원본 문서
#'
#' @examples
#' dfm <- dcm(df)
#' @export
dcm <- function(df) {
  if (is.data.frame(df)) {
    words <- word_tokenizer(df)
    news <- crossprod(table(words[2:3]))
    diag(news) <- 0
    return(as.data.frame(news))
  } else {
    stop("input type is to be have to df")
  }
}

#' word network
#'
#' 단어 네트워크 분석 결과를 시각화합니다.
#' 
#' @param dcm 단어 동시출현행렬
#' @param topwords feature 단어 갯수, 최대 50개까지 가능
#' @param min_freq 그래프를 형성할 연결 최소 빈도 수
#'
#' @examples
#' network_graph(dcm, 40, 1)
#' @import quanteda
#' @import quanteda.textplots
#' @export
network_graph <- function(dcm, topwords = 50, min_freq=0.5){
  if (topwords > 50){
    stop("topwords have to under 50")
  }
  if (is.data.frame(df)) {
    dfm <- quanteda::as.dfm(dcm)
    news_fcm <- quanteda::fcm(dfm)
    top <- names(quanteda::topfeatures(news_fcm, topwords))
    quanteda::fcm_select(news_fcm, pattern = top) |> 
      quanteda.textplots::textplot_network(min_freq = min_freq)
  }else {
    stop("input type is to be have to df(dcm)")
  }
}
