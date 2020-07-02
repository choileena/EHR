#' Standardize Route Entity
#'
#' This function standardizes the route entity.
#'
#' @param x character vector of extracted route values
#'
#' @return character vector
#'
#' @examples
#' stdzRoute(c('oral', 'po', 'subcut'))
#' @export

stdzRoute <- function(x) {
  rte <- gsub('[[:space:].]', '', tolower(x))
  urte <- unique(rte)
  useUnq <- length(urte) != length(rte)
  if(useUnq) {
    mix <- match(rte, urte)
    rte <- urte
  }
  rte[grep('skin', rte)] <- 'transdermal'
  rte[grep('iv|intravenous', rte)] <- 'iv'
  rte[grep('mouth|oral|po', rte)] <- 'orally'
  rte[grep('subcut|sq', rte)] <- 'sq'
  rte[nchar(rte) == 0] <- NA
  if(useUnq) {
    rte <- rte[mix]
  }
  rte
}
