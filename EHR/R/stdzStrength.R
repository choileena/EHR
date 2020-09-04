#' Standardize Strength Entity
#'
#' This function standardizes the strength entity.
#'
#' Some strength strings may include multiple values and additional
#' interpretation may be needed. For example \sQuote{2-1} likely indicates
#' a strength of 2 followed by a strength of 1. Thus a single element may need
#' to be standarized into two elements. This can only happen if the frequency
#' entity is missing or in agreement (\sQuote{bid} for example). See the
#' \sQuote{addl_data} attribute of the returned vector.
#'
#' @param str character vector of extracted strength values
#' @param freq character vector of extracted frequency values
#'
#' @return numeric vector
#'
#' @examples
#' stdzStrength(c('1.5', '1/2', '1/1/1'))
#' stdzStrength(c('1.5', '1/2', '1/1/1'), c('am', 'daily', NA))
#' stdzStrength(c('1.5', '1/2', '1/1/1'), FALSE)
#' @export

stdzStrength <- function(str, freq) {
  cstrg <- tolower(str)
  if(missing(freq)) {
    noFreq <- TRUE
    freq <- NA
  } else {
    noFreq <- is.na(freq) | nchar(freq) == 0
  }
  # store additional values
  addl <- vector('list', length(cstrg))
  cstrg <- sub('one', '1', cstrg)
  cstrg <- sub('two', '2', cstrg)
  cstrg <- sub('three', '3', cstrg)
  cstrg <- sub('four', '4', cstrg)
  cstrg <- sub('five', '5', cstrg)
  cstrg <- sub('six', '6', cstrg)
  cstrg <- sub('seven', '7', cstrg)
  cstrg <- sub('eight', '8', cstrg)
  cstrg <- sub('nine', '9', cstrg)
  cstrg <- sub('ten', '10', cstrg)
  cstrg <- sub('([0-9])to([0-9])', '\\1-\\2', cstrg)

  # if STR1/STR2/STR3, consider duplicate row with am/noon/pm
  ix3 <- grepl("^[0-9.]+[ ]?[-/][ ]?[0-9.]+[ ]?[-/][ ]?[0-9.]+", cstrg) & (noFreq | freq == 'tid')
  ix3 <- which(ix3)
  if(length(ix3)) {
    expr <- "^([0-9.]+)[ ]?[-/][ ]?([0-9.]+)[ ]?[-/][ ]?([0-9.]+)([^0-9.].*|$)"
    s1 <- nowarnnum(sub(expr, "\\1", cstrg[ix3]))
    s2 <- nowarnnum(sub(expr, "\\2", cstrg[ix3]))
    s3 <- nowarnnum(sub(expr, "\\3", cstrg[ix3]))
    cstrg[ix3] <- s1
    for(i in seq_along(ix3)) {
      df <- data.frame(str = c(s1[i], s2[i], s3[i]), freq = c('am','noon','pm'), stringsAsFactors = FALSE)
      addl[[ix3[i]]] <- df
    }
  }
  # if STR1/STR2, consider duplicate row with am/pm
  ix2 <- grepl("^[0-9.]+[ ]?[-/][ ]?[0-9.]+", cstrg) & (noFreq | freq == 'bid')
  ix2 <- setdiff(which(ix2), ix3)
  if(length(ix2)) {
    expr <- "^([0-9.]+)[ ]?[-/][ ]?([0-9.]+)([^0-9.].*|$)"
    csa <- nowarnnum(sub(expr, "\\1", cstrg[ix2]))
    csb <- nowarnnum(sub(expr, "\\2", cstrg[ix2]))
    cstrg[ix2] <- csa
    for(i in seq_along(ix2)) {
      df <- data.frame(str = c(csa[i], csb[i]), freq = c('am','pm'), stringsAsFactors = FALSE)
      addl[[ix2[i]]] <- df
    }
  }
  # if STR1-STR2, consider duplicate row with am/pm or average
  # or maybe just duplicate row
  dash_ix <- grep("^[0-9.]+[ ]?-[ ]?[0-9.]+", cstrg)
  dash_ix <- setdiff(dash_ix, union(ix3, ix2))
  if(length(dash_ix)) {
    expr <- "^([0-9.]+)[ ]?-[ ]?([0-9.]+)([^0-9.].*|$)"
    cs1 <- nowarnnum(sub(expr, "\\1", cstrg[dash_ix]))
    cs2 <- nowarnnum(sub(expr, "\\2", cstrg[dash_ix]))
    cstrg[dash_ix] <- cs1
    for(i in seq_along(dash_ix)) {
      df <- data.frame(str = c(cs1[i], cs2[i]))
      addl[[dash_ix[i]]] <- df
    }
  }
  # keep until non-numeric
  cstrg <- sub('^([0-9.]+)[^0-9.].*', '\\1', cstrg)
  cstrg <- nowarnnum(cstrg)
  if(any(lengths(addl) != 0)) {
    attr(cstrg, 'addl_data') <- addl
  }
  cstrg
}
