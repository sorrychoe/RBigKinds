lda <- function(dataframe, k = 10) {
  if (is.data.frame(dataframe)) {
    data <- word_tokenizer(dataframe)
    data <- data |> 
      count(키워드, 제목) |> 
      cast_dtm(키워드, 제목, n)
    model <- LDA(data, k = k)
    
    return(model)
  } else {
    stop("input type is to be have to DataFrame")
  }
}
