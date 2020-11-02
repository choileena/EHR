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
  dow <- character(length(dur))
  days <- c('sun(day)?', 'mon([^t]|$)(ay)?', 'tue(s)?(day)?', 'wed(nesday)?', 'thu(r)?(sday)?', 'fri(day)?', 'sat(urday)?')
  abbr <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')
  for(i in seq_along(days)) {
    ix <- grep(days[i], dur)
    if(length(ix)) {
      dur[ix] <- sub(days[i], '', dur[ix])
      dow[ix] <- paste0(dow[ix], abbr[i])
    }
  }
  # investigate these cases
#   ix <- which(nchar(dow) > 0 & nchar(dur) > 0)
#   if(length(ix)) {
#     udur[ix]
#   }
  ix <- which(nchar(dow) > 0)
  if(length(ix)) {
    dur[ix] <- dow[ix]
  }
  dur <- sub('(s)', '', dur, fixed = TRUE)
  dur <- sub('(each|per)', 'every', dur)
  dur <- sub('everyweek', 'every7day', dur)
  dur <- sub('weekly', 'every7day', dur)
  dur <- sub('everymonth', 'every30day', dur)
  dur <- sub('first(day|week|month)', '\\11', dur)
  dur <- sub('second(day|week|month)', '\\12', dur)
  dur <- sub('third(day|week|month)', '\\13', dur)
  dur <- sub('fourth(day|week|month)', '\\14', dur)
  dur <- sub('fifth(day|week|month)', '\\15', dur)
  dur <- sub('sixth(day|week|month)', '\\16', dur)
  dur <- sub('seventh(day|week|month)', '\\17', dur)
  dur <- sub('eighth(day|week|month)', '\\18', dur)
  dur <- sub('ninth(day|week|month)', '\\19', dur)
  dur <- sub('tenth(day|week|month)', '\\110', dur)
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
  dur <- sub('(day|week|month)\\(s\\)([0-9])', '\\1 \\2', dur)
  ix <- grep('every([0-9]+) week\\(s\\)', dur)
  if(length(ix)) {
    v <- sub('every([0-9]+) week\\(s\\)', '\\1', dur[ix])
    dur[ix] <- sprintf('every %s day(s)', as.numeric(v) * 7)
  }
  ix <- grep('every([0-9]+) month\\(s\\)', dur)
  if(length(ix)) {
    v <- sub('every([0-9]+) month\\(s\\)', '\\1', dur[ix])
    dur[ix] <- sprintf('every %s day(s)', as.numeric(v) * 30)
  }
  dur <- sub('every([0-9])', 'every \\1', dur)
  dur <- sub('(s)ly', 'ly', dur, fixed = TRUE)
  dur <- sub('^a ', '1 ', dur)
  dur <- gsub('^[ ]', '', dur)
  dur[nchar(dur) == 0] <- NA
  if(useUnq) {
    dur <- dur[mix]
  }
  dur
}
