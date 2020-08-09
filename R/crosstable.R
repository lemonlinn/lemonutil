#' Generate a table for cross tabulations
#'
#' This function generates a flextable for a cross tabulation
#' between two categorical (nominal or ordinal), numeric variables within a dataframe.
#' There is an option to make the column names of the table reflect response options
#' instead of the default, where the column names are the actual numeric values.
#'
#' @param data Object holding the dataframe
#' @param x String of the first variable name
#' @param y String of the second variable name
#' @param x_names Vector of response options. Default value is FALSE.
#' @param y_names Vector of response options. Default value is FALSE.
#' @return A flextable object
#' @examples
#' mat <- as.data.frame(matrix(1:20, 5, 4, dimnames = list(NULL, LETTERS[1:4])))
#' crosstable(mat, "A", "B")
#' crosstable(mat, "A", "B", x_names = c("one", "two", "three", "four", "five"), y_names = c("six", "seven", "eight", "nine", "ten"))
#' @export
crosstable <- function(data, x, y, x_names = FALSE, y_names = FALSE){
  tab = xtabs(formula = ~unlist(data[y])+unlist(data[x]))
  tabDF = as.data.frame.matrix(tab)

  if (!isFALSE(y_names)){
    rownames(tabDF) <- c(y_names)
  }

  tabDF <- cbind(x = rownames(tabDF), tabDF)

  if (!isFALSE(x_names)){
    colnames(tabDF) <- c(y, x_names)
  }

  chitest = summary(tab)
  title = paste0("Cross Tabulation of ", x, " and ", y)
  foot = paste0("(n=", toString(nrow(na.omit(data[c(x,y)]))), ")")
  chi = paste0("Chi-Square=", round(chitest$statistic,2), " (p-val=", round(chitest$p.value,4), ")")
  maxval = ncol(tabDF)

  ft <- flextable::flextable(tabDF)

  ft <- flextable::add_header_row(ft, top = TRUE, values = c(NA, x), colwidths = c(1, maxval-1))
  ft <- flextable::add_header_row(ft, top = TRUE, values = title, colwidths = maxval)
  ft <- flextable::align(ft, i = c(1:2), align = "center", part = "header")
  ft <- flextable::add_footer_row(ft, top = FALSE, values = c(chi, foot), colwidths = c(maxval-1, 1))
  ft <- flextable::align(ft, i = 1, j = maxval, align = "right", part = "footer")

  #ft <- border_remove(ft)
  ft <- bold(ft, part = "header", i = 1)
  #ft <- hline_bottom(ft, border = fp_border())
  #ft <- hline(ft, i = 1, border = fp_border(), part = "header")
  return(ft)
}
