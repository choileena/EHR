#' Standardize Duration Entity
#'
#' This function standardizes the duration entity.
#'
#' @param x character vector of extracted duration values
#'
#' @return character vector
#'
#' @examples
#' stdzDuration(c('1 month', 'three days', 'two-weeks'))
#' @export

stdzDuration <- function(x) {
  dur <- gsub('[[:space:]]', '', tolower(x))
  udur <- unique(dur)
  useUnq <- length(udur) != length(dur)
  if(useUnq) {
    mix <- match(dur, udur)
    dur <- udur
  }
  dur <- sub('one', 1, dur)
  dur <- sub('two', 2, dur)
  dur <- sub('three', 3, dur)
  dur <- sub('four', 4, dur)
  dur <- sub('five', 5, dur)
  dur <- sub('six', 6, dur)
  dur <- sub('seven', 7, dur)
  dur <- sub('eight', 8, dur)
  dur <- sub('nine', 9, dur)
  dur <- sub('ten', 10, dur)
  dur <- sub('-?(hour|hr)s?', ' hour(s)', dur)
  dur <- sub('-?days?', ' day(s)', dur)
  dur <- sub('-?(week|wk)s?', ' week(s)', dur)
  dur <- sub('-?(month|mth)s?', ' month(s)', dur)
  dur <- sub('-?(year|yr)s?', ' year(s)', dur)
  dur <- sub('^a ', '1 ', dur)
  dur[nchar(dur) == 0] <- NA
  if(useUnq) {
    dur <- dur[mix]
  }
  dur
}
