#' Standardize Dose Change Entity
#'
#' This function standardizes the dose change entity.
#'
#' @param x character vector of extracted dose change values
#'
#' @return character vector
#'
#' @examples
#' stdzDoseChange(c('decreasing','dropped','increased'))
#' @export

stdzDoseChange <- function(x) {
  dc <- gsub("[[:space:]]", "", tolower(x))
  udc <- unique(dc)
  useUnq <- length(udc) != length(dc)
  if(useUnq) {
    mix <- match(dc, udc)
    dc <- udc
  }
  dc[grep("inc", dc)] <- '+'
  dc[grep("dec|down|drop|lower|reduc", dc)] <- '-'
  dc[nchar(dc) == 0] <- NA
  if(useUnq) {
    dc <- dc[mix]
  }
  dc
}
