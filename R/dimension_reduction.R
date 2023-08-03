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