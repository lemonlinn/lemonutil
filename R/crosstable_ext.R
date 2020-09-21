#' Generates a table for cross tabulations with percentages
#'
#' This function generates a flextable for a cross tabulation
#' between two categorical (nominal or ordinal), numeric variables within a dataframe.
#' The cell values contains the total percentage, column percentage, row percentage, and count.
#' There is an option to make the column names of the table reflect response options
#' instead of the default, where the column names are the actual numeric values.
#'
#' @param data Object holding the dataframe
#' @param x String of the first variable name
#' @param y String of the second variable name
#' @param x_names Vector of response options. Default value is FALSE.
#' @param y_names Vector of response options. Default value is FALSE.
#' @param percents String representing what percentages to calculate. Accepts any of c("TRC", "RC", "TC", "TR", "C", "R", "T"). Default is "TRC".
#' @param row_t Boolean determining if row totals should be included. Default value is TRUE.
#' @param col_t Boolean determining if column totals should be included. Default value is TRUE.
#' @return A flextable object
#' @examples
#' mat <- as.data.frame(matrix(1:20, 5, 4, dimnames = list(NULL, LETTERS[1:4])))
#' crosstable(mat, "A", "B")
#' crosstable(mat, "A", "B", x_names = c("one", "two", "three", "four", "five"), y_names = c("six", "seven", "eight", "nine", "ten"))
#' @export
crosstable_ext <- function(data, x, y, x_names = FALSE, y_names = FALSE, percents = "TRC", row_t = TRUE, col_t = TRUE){
  tab = xtabs(formula = ~unlist(data[y])+unlist(data[x]))
  tabDF = as.data.frame.matrix(tab)
  oldrownames = rownames(tabDF)
  oldcolnames = colnames(tabDF)
  N = nrow(na.omit(data[c(x,y)]))
  C = colSums(tabDF, na.rm = T)
  R = rowSums(tabDF, na.rm = T)

  if (percents != "N"){
    for (i in 1:ncol(tabDF)){
      for (j in 1:nrow(tabDF)){
        a = strtoi(tabDF[j,i])
        if (percents == "TRC"){
          tabDF[j,i] <- paste0(toString(round((a/N)*100, digits=2)), "%T", "\n",
                               toString(round((a/C[i])*100,digits=2)), "%C", "\n",
                               toString(round((a/R[j])*100,digits=2)), "%R", "\n",
                               "(n=", toString(a), ")")
        } else if (percents == "RC"){
          tabDF[j,i] <- paste0(toString(round((a/C[i])*100,digits=2)), "%C", "\n",
                               toString(round((a/R[j])*100,digits=2)), "%R", "\n",
                               "(n=", toString(a), ")")
        } else if (percents == "TR"){
          tabDF[j,i] <- paste0(toString(round((a/N)*100, digits=2)), "%T", "\n",
                               toString(round((a/R[j])*100,digits=2)), "%R", "\n",
                               "(n=", toString(a), ")")
        } else if (percents == "TC"){
          tabDF[j,i] <- paste0(toString(round((a/N)*100, digits=2)), "%T", "\n",
                               toString(round((a/C[i])*100,digits=2)), "%C", "\n",
                               "(n=", toString(a), ")")
        } else if (percents == "T"){
          tabDF[j,i] <- paste0(toString(round((a/N)*100, digits=2)), "%T", "\n",
                               "(n=", toString(a), ")")
        } else if (percents == "R"){
          tabDF[j,i] <- paste0(toString(round((a/R[j])*100,digits=2)), "%R", "\n",
                               "(n=", toString(a), ")")
        } else if (percents == "C"){
          tabDF[j,i] <- paste0(toString(round((a/C[i])*100,digits=2)), "%C", "\n",
                               "(n=", toString(a), ")")
        }
      }
    }
  }

  if (row_t & col_t){
    tabDF <- cbind(tabDF, data.frame("Row Totals" = R))
    colDF <- data.frame(t(c(C, paste0("N=", toString(N)))))
    colnames(colDF) <- colnames(tabDF)
    tabDF <- rbind(tabDF, colDF)
    if (!isFALSE(y_names)){
      rownames(tabDF) <- c(y_names, "Column Totals")
    } else {
      rownames(tabDF) <- c(oldrownames, "Column Totals")
    }
    tabDF <- cbind(x = rownames(tabDF), tabDF)
    if (!isFALSE(x_names)){
      colnames(tabDF) <- c(y, x_names, "Row Totals")
    } else {
      colnames(tabDF) <- c(y, oldcolnames, "Row Totals")
    }
  }

  if (row_t & !col_t){
    tabDF <- cbind(tabDF, data.frame("Row Totals" = R))
    if (!isFALSE(y_names)){
      rownames(tabDF) <- c(y_names)
    } else {
      rownames(tabDF) <- c(oldrownames)
    }
    tabDF <- cbind(x = rownames(tabDF), tabDF)
    if (!isFALSE(x_names)){
      colnames(tabDF) <- c(y, x_names, "Row Totals")
    } else {
      colnames(tabDF) <- c(y, oldcolnames, "Row Totals")
    }
  }

  if (!row_t & col_t){
    colDF <- data.frame(t(C))
    colnames(colDF) <- colnames(tabDF)
    tabDF <- rbind(tabDF, colDF)
    if (!isFALSE(y_names)){
      rownames(tabDF) <- c(y_names, "Column Totals")
    } else {
      rownames(tabDF) <- c(oldrownames, "Column Totals")
    }
    tabDF <- cbind(x = rownames(tabDF), tabDF)
    if (!isFALSE(x_names)){
      colnames(tabDF) <- c(y, x_names)
    } else {
      colnames(tabDF) <- c(y, oldcolnames)
    }
  }

  if (!row_t & !col_t){
    if (!isFALSE(y_names)){
      rownames(tabDF) <- c(y_names)
    } else {
      rownames(tabDF) <- c(oldrownames)
    }
    tabDF <- cbind(x = rownames(tabDF), tabDF)
    if (!isFALSE(x_names)){
      colnames(tabDF) <- c(y, x_names)
    } else {
      colnames(tabDF) <- c(y, oldcolnames)
    }
  }

  chitest = summary(tab)
  title = paste0("Cross Tabulation of ", x, " and ", y)
  foot = paste0("(N=", toString(N), ")")
  chi = paste0("Chi-Square=", round(chitest$statistic,2), " (p-val=", round(chitest$p.value,4), ")")
  maxval = ncol(tabDF)

  ft <- flextable::flextable(tabDF)
  ft <- flextable::add_header_row(ft, top = TRUE, values = c(NA, x), colwidths = c(1, maxval-1))
  ft <- flextable::add_header_row(ft, top = TRUE, values = title, colwidths = maxval)
  ft <- flextable::align(ft, i = c(1:2), align = "center", part = "header")
  if (col_t & row_t){
    ft <- flextable::add_footer_row(ft, top = FALSE, values = c(chi, " "), colwidths = c(maxval-1, 1))
  } else {
    ft <- flextable::add_footer_row(ft, top = FALSE, values = c(chi, foot), colwidths = c(maxval-1, 1))
  }
  ft <- flextable::align(ft, i = 1, j = maxval, align = "right", part = "footer")
  ft <- flextable::bold(ft, part = "header", i = 1)
  return(ft)
}
