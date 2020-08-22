#' Convert a flextable object to a dataframe
#'
#' This function takes a flextable object as input and returns a data.frame object
#' containing the content of the flextable cells.
#'
#' @param flex A flextable object
#' @param tbl_type String referring to type of table. Accepts "unitable" or "crosstable".
#' @return A data.frame object
#'
#' @export
flex2df <- function(flex, tbl_type){
  if (tbl_type == "unitable"){
    tmp_perc <- c(as.character(unlist(flex$body$dataset[1,])))
    tmp_count <- c(as.character(unlist(flex$body$dataset[2,])))
    tmpdf <- data.frame("percentage"=tmp_perc, "count"=tmp_count)
    tmpdf <- tmpdf[2:nrow(tmpdf),]
    rownames(tmpdf) <- NULL

    tmpdf[1] <- sapply(tmpdf[1], FUN = sub, pattern="%", replacement="")
    tmpdf[1] <- sapply(tmpdf[1], FUN = as.numeric)
    tmpdf[1] <- tmpdf[1]/100

    tmpdf[2] <- sapply(tmpdf[2], FUN = sub, pattern="\\(n=", replacement="")
    tmpdf[2] <- sapply(tmpdf[2], FUN = sub, pattern="\\)", replacement="")
    tmpdf[2] <- sapply(tmpdf[2], FUN = as.numeric)
    return(tmpdf)
  } else if (tdl_type == "crosstable"){

  }
}
