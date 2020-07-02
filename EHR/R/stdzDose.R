#' Standardize Dose Entity
#'
#' This function standardizes the dose entity.
#'
#' Some dose strings may include multiple values and additional interpretation
#' may be needed. For example \sQuote{2-1} likely indicates a dose of 2 followed
#' by a dose of 1. Currently it would be converted to the average of 1.5.
#'
#' @param x character vector of extracted dose values
#'
#' @return numeric vector
#'
#' @examples
#' stdzDose(c('one tablet', '1/2 pill', '1-3 tabs'))
#' @export

stdzDose <- function(x) {
  cdose <- tolower(x)
  udose <- unique(cdose)
  useUnq <- length(udose) != length(cdose)
  if(useUnq) {
    mix <- match(cdose, udose)
    cdose <- udose
  }
  cdose <- sub('[ ]*(cap|capsule|tablet|tab|pill)[s]?', '', cdose)
  cdose <- sub('one', 1, cdose)
  cdose <- sub('two', 2, cdose)
  cdose <- sub('three', 3, cdose)
  cdose <- sub('four', 4, cdose)
  cdose <- sub('five', 5, cdose)
  cdose <- sub('six', '6', cdose)
  cdose <- sub('seven', '7', cdose)
  cdose <- sub('eight', '8', cdose)
  cdose <- sub('nine', '9', cdose)
  cdose <- sub('ten', '10', cdose)
  cdose <- sub('half', 0.5, cdose, fixed = TRUE)
  cdose <- sub('1-2', 1.5, cdose, fixed = TRUE)
  cdose <- sub('1-1/2', 1.5, cdose, fixed = TRUE)
  cdose <- sub('1/2', 0.5, cdose, fixed = TRUE)
  ix <- grep("[0-9][ ]?(to|-)[ ]?[0-9]", cdose)
  # if DOSE1-DOSE2, take the average
  # another option is to duplicate row and include both dose amounts
  if(length(ix)) {
    cda <- sub("([0-9.]+)[ ]?(to|-)[ ]?([0-9.]+)", "\\1", cdose[ix])
    cdb <- sub("([0-9.]+)[ ]?(to|-)[ ]?([0-9.]+)", "\\3", cdose[ix])
    cdose[ix] <- sprintf("%.2f", (as.numeric(cda) + as.numeric(cdb)) / 2)
  }
  cdose <- nowarnnum(cdose)
  if(useUnq) {
    cdose <- cdose[mix]
  }
  cdose
}
