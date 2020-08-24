#' Generate a ggplot2 box and whisker plot using mean and standard deviations
#'
#' This function takes two columns from a data.frame object and generates a ggplot2 object. The
#' ggplot2 object is a box and whisker plot where the median line is replaced with the mean, the
#' first and third quartile box is replaced with the first standard deviation above and below the mean,
#' and the whiskers are replaced with the minimum and maximum values of the data. Variables containing
#' the output of this function can be treated as the root for further ggplot2 additions (ex. labs,
#' geom_jitter).
#'
#' @param x_var A categorical variable for the x axis
#' @param y_var A continuous or semi-continuous variable for the y axis
#' @return A ggplot2 graph object
#'@export
meanBoxplot <- function(x_var, y_var) {
  ggplot(d, aes(factor(x_var), y_var)) +
    stat_summary(fun.data=MinMeanSEMMax, geom="boxplot") +
    ggtitle("Boxplot: Min, Mean-1SEM, Mean, Mean+1SEM, Max")
}

MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}
