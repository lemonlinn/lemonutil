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
#' unitable(mat, mat$A, 1, 5, c("A", "one", "two", "three", "four", "five"), "What is the question?", "Example table")
#' @export
unitable <- function(data, colval, minval = FALSE, maxval = FALSE, namecol = FALSE, text = " ", title = FALSE) {
  if (isFALSE(minval)){
    colval = unlist(data[colval])
    minval = min(na.omit(colval))
  }
  if (isFALSE(maxval)){
    maxval = max(na.omit(colval))
  }
  myvec <- vector("character", length = length(minval:maxval))
  myprop <- vector("character", length = length(minval:maxval))
  myvals <- c(minval:maxval)
  for (val1 in 1:length(myvals)){
    myvec[val1] <- paste("(n=",toString(nrow(data[(colval == myvals[val1]), ])),")", sep = "")
    myprop[val1] <- sprintf("%0.1f%%",(nrow(data[(colval == myvals[val1]), ])/length(colval))*100)
  }
  myprop <- append(myprop, text, after = 0)
  myvec <- append(myvec, paste("(N=", toString(length(colval)), ")", sep = ""), after = 0)
  mydf <- data.frame(t(data.frame(myprop, myvec)))

  if (!isFALSE(namecol)){
    names(mydf) <- c(" ", namecol)
  } else{
    names(mydf) <- c(" ", minval:maxval)
  }

  num_i <- 1
  ft <- flextable::flextable(mydf)
  if (!isFALSE(title)){
    ft <- flextable::add_header_row(ft, top = TRUE, values = title, colwidths = c(length(myvals)+1))
    ft <- flextable::align(ft, i = 1, align = "center", part = "header")
    num_i <- 2
  }
  ft <- flextable::autofit(ft)
  ft <- flextable::border_remove(ft)
  ft <- flextable::border_inner_v(ft, border = officer::fp_border(), part = "body")
  ft <- flextable::border(ft, i = num_i, j = c(1:length(myvals)), border.right = officer::fp_border(), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border())
  ft <- flextable::hline(ft, i = num_i, j = c(1:length(myvals)+1), border = officer::fp_border(), part = "header")
  ft <- flextable::bold(ft, i = c(1:num_i), part = 'header')
  return(ft)
}

# unitable <- function(data, colval, minval = FALSE, maxval = FALSE, namecol = FALSE, text = " ", title = FALSE) {
#   if (isFALSE(minval)){
#     minval = min(na.omit(colval))
#   }
#   if (isFALSE(maxval)){
#     maxval = max(na.omit(colval))
#   }
#   myvec <- vector("character", length = length(minval:maxval))
#   myprop <- vector("character", length = length(minval:maxval))
#   myvals <- c(minval:maxval)
#   for (val1 in 1:length(myvals)){
#     myvec[val1] <- paste("(n=",toString(nrow(data[(colval == myvals[val1]), ])),")", sep = "")
#     myprop[val1] <- sprintf("%0.1f%%",(nrow(data[(colval == myvals[val1]), ])/length(colval))*100)
#   }
#   myprop <- append(myprop, text, after = 0)
#   myvec <- append(myvec, paste("(N=", toString(length(colval)), ")", sep = ""), after = 0)
#   mydf <- data.frame(t(data.frame(myprop, myvec)))
#
#   if (!isFALSE(namecol)){
#     names(mydf) <- c(" ", namecol)
#   } else{
#     names(mydf) <- c(" ", minval:maxval)
#   }
#
#   num_i <- 1
#   ft <- flextable::flextable(mydf)
#   if (!isFALSE(title)){
#     ft <- flextable::add_header_row(ft, top = TRUE, values = title, colwidths = c(length(myvals)+1))
#     ft <- flextable::align(ft, i = 1, align = "center", part = "header")
#     num_i <- 2
#   }
#   ft <- flextable::autofit(ft)
#   ft <- flextable::border_remove(ft)
#   ft <- flextable::border_inner_v(ft, border = officer::fp_border(), part = "body")
#   ft <- flextable::border(ft, i = num_i, j = c(1:length(myvals)), border.right = officer::fp_border(), part = "header")
#   ft <- flextable::hline_bottom(ft, border = officer::fp_border())
#   ft <- flextable::hline(ft, i = num_i, j = c(1:length(myvals)+1), border = officer::fp_border(), part = "header")
#   ft <- flextable::bold(ft, i = c(1:num_i), part = 'header')
#   return(ft)
# }
