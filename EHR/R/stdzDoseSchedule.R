#' Standardize Dose Schedule Entity
#'
#' This function standardizes the dose schedule entity.
#'
#' @param x character vector of extracted dose schedule values
#'
#' @return character vector
#'
#' @examples
#' stdzDoseSchedule(c('tapered','weaned','TAPER'))
#' @export

stdzDoseSchedule <- function(x) {
  ds <- tolower(x)
  uds <- unique(ds)
  useUnq <- length(uds) != length(ds)
  if(useUnq) {
    mix <- match(ds, uds)
    ds <- uds
  }
  ds[grep("taper|wean", ds)] <- 'taper'
  ds[grep("off|stop|done|gone", ds)] <- 'stop'
  ds[grep("alternat", ds)] <- 'alternate'
  ds[nchar(ds) == 0] <- NA
  if(useUnq) {
    ds <- ds[mix]
  }
  ds
}
