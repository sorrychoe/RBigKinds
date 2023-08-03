day_range <- function(df) {
  if (is.data.frame(df)) {
    print(paste("first day: ", min(df$일자)))
    print(paste("last day: ", max(df$일자)))
  } else {
    stop("input type is to be have to DataFrame")
  }
}