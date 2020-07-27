#' Data Transformation
#'
#' Convenience function for making small modifications to a data.frame.
#'
#' @param x a data.frame
#' @param select columns to select
#' @param rename character vector with names for all columns
#' @param modify list of expressions used to transform data set
#'
#' @return The modified data.frame
#' 
#' @export

dataTransformation <- function(x, select, rename, modify) {
  if(!missing(select)) {
    x <- x[,select]
  }
  if(!missing(rename)) {
    ix <- seq_along(rename)
    if(length(names(rename))) {
      ix <- match(names(rename), names(x))
    }
    if(sum(!is.na(ix)) != length(rename)) stop('column rename failed')
    names(x)[ix] <- rename
  }
  if(!missing(modify)) {
    cname <- names(modify)
    for(i in seq_along(modify)) {
      val <- eval(modify[[i]], x)
      x[,cname[i]] <- val
    }
  }
  x
}

#' Read and Transform
#'
#' Convenience function for reading in a CSV file, and making small
#' modifications to a data.frame.
#'
#' If \code{\link{read.csv}} needs additional arguments (or the file is in a
#' different format), the user should load the data first, then directly call
#' \code{\link{dataTransformation}}.
#'
#' @param file filename of a CSV file
#' @param \dots additional information passed to \code{\link{dataTransformation}}
#'
#' @return The modified data.frame
#' 
#' @export

readTransform <- function(file, ...) {
  x <- read.csv(file, stringsAsFactors = FALSE)
  if(length(list(...))) {
    x <- dataTransformation(x, ...)
  }
  x
}
