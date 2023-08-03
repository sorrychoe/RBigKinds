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