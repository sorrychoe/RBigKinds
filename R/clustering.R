kmeans <- function(vec, k, random_state = 123) {
  if (is.matrix(vec)) {
    set.seed(random_state)
    kmeans_model <- kmeans(vec, centers = k, iter.max = 1000)
    return(kmeans_model$cluster)
  } else {
    stop("input type is to be have to matrix")
  }
}

dbscan <- function(vec, eps, min_samples, metric = "euclidean") {
  if (is.matrix(vec)) {
    dbscan_model <- dbscan::dbscan(vec, eps = eps, minPts = min_samples, method = metric)
    return(dbscan_model$cluster)
  } else {
    stop("input type is to be have to matrix")
  }
}

meanshift <- function(vec, qt = 0.25) {
  if (is.matrix(vec)) {
    best_bandwidth <- density(vec)$bw
    print(paste(qt, "기준 최적 bandwidth 값:", round(best_bandwidth, 2)))
    
    ms_model <- meanshift(vec, bandwidth = best_bandwidth)
    print(paste("cluster 갯수:", length(unique(ms_model))))
    return(ms_model)
  } else {
    stop("input type is to be have to matrix")
  }
}