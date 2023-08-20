#' Kmeans
#'
#' kmeans clustering을 진행합니다.
#' 
#' @param df BigKinds 원본 문서
#' @param k  형성할 군집 갯수
#' @param random_state seed 값
#'
#' @examples
#' Kmeans(df, k = 3)
#' 
#' @export
Kmeans <- function(vec, k, random_state = 123) {
  if (is.matrix(vec)) {
    set.seed(random_state)
    kmeans_model <- kmeans(vec, centers = k, iter.max = 1000)
    return(kmeans_model)
  } else {
    stop("input type is to be have to matrix")
  }
}

#' DBSCAN
#'
#' DBSCAN 알고리즘을 진행합니다.
#' 
#' @param df BigKinds 원본 문서
#' @param eps epsilon 값(보폭)
#' @param min_sample 최적 샘플 갯수
#' @param metric 거리 계산 방법(default = euclidean)
#'
#' @examples
#' DBSCAN(vec, eps = 0.5, min_sample = 3)
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