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