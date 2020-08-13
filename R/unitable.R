#' Generate a table for a single variable
#'
#' This function generates a flextable for the distribution of responses to a
#' single categorical (nominal or ordinal) numeric variable of a dataframe.
#'
#' @param data Object holding the dataframe
#' @param colval Call to the variable in the dataframe formatted as df$colname
#' @param minval Optional. Integer representing the smallest possible numeric value in the variable
#' @param maxval Optional. Integer representing the largest possible numeric value in the variable
#' @param namecol Optional. Vector containing string of response options.
#' @param text Optional. String of the question wording in the survey
#' @param title Optional. String of the table title, usually the variable name in the dataframe
#' @return A flextable object
#' @examples
#' mat <- as.data.frame(matrix(1:20, 5, 4, dimnames = list(NULL, LETTERS[1:4])))
#' unitable(mat, mat$A, 1, 5, c("A", "one", "two", "three", "four", "five"), "What is the question?", 1, "Example table")
#' @export
unitable <- function(data, colval, minval = NA, maxval = NA, namecol = NA, text = " ", title = NA) {
  myvec <- vector("character", 1)
  myprop <- vector("character", 1)
  if (minval == NA){
    minval = min(colval)
  }
  if (maxval == NA){
    maxval = max(colval)
  }
  for (val1 in minval:maxval){
    myvec[val1] <- paste("(n=",toString(nrow(data[(colval == val1), ])),")", sep = "")
    myprop[val1] <- sprintf("%0.1f%%",(nrow(data[(colval == val1), ])/length(colval))*100)
  }
  myprop <- append(myprop, text, after = 0)
  myvec <- append(myvec, paste("(n=", toString(length(colval)), ")", sep = ""), after = 0)
  mydf <- data.frame(t(data.frame(myprop, myvec)))
  if (namecol != NA){
    names(mydf) <- c("",namecol)
  }

  ft <- flextable(mydf)
  if (title != NA){
    ft <- add_header_row(ft, top = TRUE, values = title, colwidths = c(maxval+1))
  }
  #ft <- add_header_row(ft, top = TRUE, values = paste("Table #", toString(tblnum), sep = ""), colwidths = c(maxval+1))
  ft <- align(ft, i = c(1:2), align = "center", part = "header")
  #ft <- width(ft, j = c(1), width = 2.5)
  #ft <- height(ft, i = c(1), height = 2, part = "body")
  ft <- autofit(ft)
  ft <- border_remove(ft)
  ft <- border_inner_v(ft, border = fp_border(), part = "body")
  ft <- border(ft, i = 3, j = c(1:maxval), border.right = fp_border(), part = "header")
  ft <- hline_bottom(ft, border = fp_border())
  ft <- hline(ft, i = 3, j = c(1:maxval+1), border = fp_border(), part = "header")
  ft <- bold(ft, i = c(1:3), part = 'header')
  return(ft)
}
