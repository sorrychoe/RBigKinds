library(tidyverse)
library(arules)
library(proxy)

source(global.R)

day_range <- function(df) {
  if (is.data.frame(df)) {
    print(paste("first day: ", min(df$일자)))
    print(paste("last day: ", max(df$일자)))
  } else {
    stop("input type is to be have to DataFrame")
  }
}

press_counter <- function(df) {
  if (is.data.frame(df)) {
    freq <- table(df$언론사)
    brod_df <- data.frame(언론사 = names(freq), 기사 = as.numeric(freq))
    return(brod_df)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

pca <- function(vec, Random_State = 123) {
  if (is.matrix(vec)) {
    pca_df <- prcomp(vec, center = TRUE)$x[, 1:2]
    pca_df <- data.frame(`component 0` = pca_df[, 1], `component 1` = pca_df[, 2])
    return(pca_df)
  } else {
    stop("input type is to be have to matrix")
  }
}

nmf <- function(vec, Random_State = 123) {
  if (is.matrix(vec)) {
    nmf_df <- NMF::nmf(vec, 2, seed = Random_State)$W
    nmf_df <- data.frame(`component 0` = nmf_df[, 1], `component 1` = nmf_df[, 2])
    return(nmf_df)
  } else {
    stop("input type is to be have to matrix")
  }
}

t_sne <- function(vec, learn_Rate = 100) {
  if (is.matrix(vec)) {
    tsne_df <- Rtsne::Rtsne(vec, dims = 2, perplexity = learn_Rate)$Y
    tsne_df <- data.frame(`component 0` = tsne_df[, 1], `component 1` = tsne_df[, 2])
    return(tsne_df)
  } else {
    stop("input type is to be have to matrix")
  }
}

lsa <- function(vec) {
  if (is.matrix(vec)) {
    svd_df <- svd(vec)$u[, 1:2]
    svd_df <- data.frame(`component 0` = svd_df[, 1], `component 1` = svd_df[, 2])
    return(svd_df)
  } else {
    stop("input type is to be have to matrix")
  }
}

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

lda <- function(dataframe, k = 10, train = 100, fit = 10) {
  if (is.data.frame(dataframe)) {
    lis <- keyword_parser(keyword_list(dataframe))
    model <- LDA(lis, k = k)

    for (words in lis) {
      model$add.documents(words)
    }

    for (i in seq(0, train, fit)) {
      model$train(fit)
    }

    return(model)
  } else {
    stop("input type is to be have to DataFrame")
  }
}

association <- function(dataframe, min_support = 0.5, use_colnames = TRUE, min_threshold = 0.1, metric = "confidence") {
  if (is.data.frame(dataframe)) {
    words <- keyword_parser(keyword_list(dataframe))
    te_data <- as(words, "transactions")
    result <- apriori(te_data, parameter = list(supp = min_support, minlen = 2, maxlen = Inf, target = "rules"))
    result <- as.data.frame(inspect(result))
    result <- result[, c("lhs", "rhs", "support", metric)]
    colnames(result) <- c("lhs", "rhs", "support", metric)
    result <- result[result[, metric] > min_threshold, ]
    return(result)
  } else {
    stop("input type is to be have to DataFrame")
  }
}
