scatterplot <- function(df, label) {
  if (is.data.frame(df)) {
    ggplot(df, aes(component.0, component.1, color = label)) +
      geom_point() +
      labs(x = "component 0", y = "component 1") +
      ggtitle("Scatter plot for dimension reduction") +
      theme(legend.position = "bottom")
  } else {
    stop("input type is to be have to DataFrame")
  }
}