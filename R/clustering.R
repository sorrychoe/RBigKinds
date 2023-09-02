#' Kmeans
#'
#' kmeans clustering을 진행합니다.
#' 
#' @param vec 텍스트 벡터
#' @param k  형성할 군집 갯수
#' @param max 최대 반복 횟수
#' @param random_state seed 값
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
#' vec <- tfidf_vector(df)
#' 
#' cluster <- Kmeans(vec, k = 3, max = 1000)
#' cluster$cluster
#' 
#' @export
Kmeans <- function(vec, k, max = 1000, random_state = 123) {
  if (is.matrix(vec)) {
    set.seed(random_state)
    kmeans_model <- kmeans(vec, centers = k, iter.max = max)
    return(kmeans_model)
  } else {
    stop("input type is to be have to matrix")
  }
}

#' DBSCAN
#'
#' DBSCAN 알고리즘을 진행합니다.
#' 
#' @param vec 텍스트 벡터
#' @param eps epsilon 값(보폭)
#' @param min_sample 최적 샘플 갯수
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
#' vec <- tfidf_vector(df)
#' 
#' vec <- tfidf_vector(df)
#' cluster <- DBSCAN(vec, eps = 0.1, min_sample = 2)
#' 
#' @import dbscan
#' 
#' @export
DBSCAN <- function(vec, eps, min_samples) {
  if (is.matrix(vec)) {
    dbscan_model <- dbscan::dbscan(vec, eps = eps, minPts = min_samples)
    cat(paste("cluster 갯수:", length(unique(dbscan_model$cluster))),"\n\n")
    return(dbscan_model)
  } else {
    stop("input type is to be have to matrix")
  }
}