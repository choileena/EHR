#' Internal functions for collapseDose process
#'
#' These internal functions aid the main functions used in the collapse
#' process (\code{\link{collapseDose}}, \code{\link{makeDose}}).
#' 
#' \code{qrbind}: fast version of rbind.data.frame
#' 
#' \code{nowarnnum}: converts a variable to numeric and suppresses warnings
#' 
#' \code{most}: returns the mode
#' 
#' \code{pairDay}: calculates daily dose for dose sequences
#' 
#' \code{mergeAdjacent}: collapses adjacent rows that have a drug name change
#' 
#' \code{borrowFreqDoseSeq}: imputes a frequency of 'pm' if the preceding row has a frequency of 'am' or 'noon' and there is a 'pm' elsewhere in the note
#' 
#' \code{borrowVal}: borrow a unique value within a drug mention
#' 
#' \code{reOrder}: order rows
#' 
#' \code{borrowWithinDoseSeq}: borrow values within a dose sequence
#' 
#' \code{calcDailyDose}: calculates daily dose
#' 
#' \code{daySeqDups}: look for duplicate dose sequence 
#' 
#' \code{rmDuplicates}: removes redundant rows
#'
#' @name collapse-internal
#' @aliases qrbind nowarnnum most pairDay
#' mergeAdjacent borrowFreqDoseSeq borrowVal reOrder
#' borrowWithinDoseSeq calcDailyDose daySeqDups rmDuplicates
#' @keywords internal
NULL

# faster (unsafe) version of rbind.data.frame
# thanks to Patrick Aboyoun for example stripped from bioconductor's IRanges package
# https://r.789695.n4.nabble.com/combining-large-list-of-data-frames-td4573033.html
qrbind <- function(..., deparse.level=1) {
# conceivable add support for Date/times
#   setMethod("coerce", signature(from = "character", to = "Date"), function(from, to) {
#     as.Date(from)
#   })
#   setMethod("coerce", signature(from = "character", to = "POSIXct"), function(from, to) {
#     as.POSIXct(from)
#   })
  args <- list(...)
  # df must be first non-NULL data.frame
#   df <- args[[1L]]
  df <- NULL
  for(i in seq_along(args)) {
    if(!is.null(args[[i]])) {
      df <- args[[i]]
      break
    }
  }
  if(is.null(df)) return(NULL)
  cn <- colnames(df)
  class1 <- function(x) class(x)[1]
  cl <- unlist(lapply(as.list(df, use.names = FALSE), class1))
  cols <- lapply(seq_len(length(df)), function(i) {
    cols <- lapply(args, `[[`, cn[i])
    combined <- do.call(c, unname(cols))
    methods::as(combined, cl[i])
  })
  names(cols) <- colnames(df)
  do.call(data.frame, cols)
}

nowarnnum <- function(x) suppressWarnings(as.numeric(x))

most <- function(x) {
  x <- x[!is.na(x) & x != '']
  if(length(x) == 0) return(NULL)
  if(length(x) == 1) return(x)
  tx <- table(x)
  ix <- which(tx == max(tx))
  if(length(ix) > 1) return(NULL)
  names(tx)[ix]
}

pairDay <- function(x) {
  # x is data split by key1 (grid|date|note)
  # currently, only valid intaketimes are AM/NOON/PM
  nr <- nrow(x)
  df <- x[['freq']]
  dv <- x[['dose.intake']] * x[['freq.num']]
  # should always be more than one row
  if(nr == 1) {
    return(cbind(NA, dv[1]))
  }
  cnt <- 1
  ds <- rep(NA_real_, nr)
  dd <- rep(NA_real_, nr)
  # special case, unique AM/PM that appear consecutively in any order
  aix <- which(df == 'am')
  pix <- which(df == 'pm')
  nix <- which(df == 'noon')
  if(length(aix) == 1 && length(pix) == 1 && length(nix) == 0 && abs(aix - pix) == 1) {
    ix <- sort(c(aix, pix))
    ds[ix] <- seq(2)
    dd <- dv
    dd[ix] <- sum(dv[ix])
    return(cbind(ds, dd))
  }
  # special case, unique AM/NOON/PM that appear consecutively in any order
  if(length(aix) == 1 && length(pix) == 1 && length(nix) == 1) {
    ix <- sort(c(aix, pix, nix))
    if(all(diff(ix) == c(1,1))) {
      ds[ix] <- seq(3)
      dd <- dv
      dd[ix] <- sum(dv[ix])
      return(cbind(ds, dd))
    }
  }
  # non-unique cases must appear in order (AM < NOON < PM)
  while(cnt < nr - 1) {
    if(df[cnt] == 'am' && df[cnt+1] == 'noon' && df[cnt+2] == 'pm') {
      ix <- c(cnt, cnt + 1, cnt + 2)
    } else if(df[cnt] == 'am' && df[cnt+1] == 'noon' && df[cnt+2] != 'pm') {
      ix <- c(cnt, cnt + 1)
    } else if(df[cnt] == 'am' && df[cnt+1] == 'pm') {
      ix <- c(cnt, cnt + 1)
    } else if(df[cnt] == 'noon' && df[cnt+1] == 'pm') {
      ix <- c(cnt, cnt + 1)
    } else if(df[cnt] == 'am' && df[cnt+1] != 'noon' && df[cnt+1] != 'pm') {
      ix <- cnt
    } else if(df[cnt] == 'noon' && df[cnt+1] != 'pm') {
      ix <- cnt
    } else if(df[cnt] == 'pm') {
      ix <- cnt
    } else {
      ix <- cnt
    }
    l <- length(ix)
    if(l == 1) {
      ds[ix] <- NA
    } else {
      ds[ix] <- seq(l)
    }
    dd[ix] <- sum(dv[ix])
    cnt <- cnt + l
  }
  if(cnt == nr - 1) {
    ix <- c(cnt, cnt + 1)
    if(df[cnt] == 'am' && df[cnt+1] %in% c('noon','pm')) {
      ds[ix] <- seq(2)
      dd[ix] <- sum(dv[ix])
    } else if(df[cnt] == 'noon' && df[cnt+1] == 'pm') {
      ds[ix] <- seq(2)
      dd[ix] <- sum(dv[ix])
    } else {
      ds[ix] <- NA
      dd[ix] <- dv[ix]
    }
  } else if(cnt == nr) {
    ds[cnt] <- NA
    dd[cnt] <- dv[cnt]
  }
  cbind(ds, dd)
}

mergeAdjacent <- function(xx, verifyCols) {
  # if second row has strength -- do not collapse
  if(!is.na(xx[2,'strength.num'])) return(NULL)
  # each column in verifyCols should contain at most 1 value for collapse to occur
  vals <- lapply(xx[,verifyCols], function(i) i[!is.na(i) & nchar(i) > 0])
  ls <- vapply(vals, length, numeric(1))
  # route is an exception - it may contain at most 1 unique value
  if('route' %in% verifyCols) {
    lr <- length(unique(vals[['route']]))
    ls[match('route', verifyCols)] <- lr
  }
  if(any(ls > 1)) return(NULL)
  coll <- names(ls[ls == 1])
  for(i in coll) {
    xx[,i] <- vals[[i]]
  }
  # provide illegal value to remove
  xx[1,'strength.num'] <- -999
  xx
}

borrowFreqDoseSeq <- function(xx) {
  f <- xx[,'freq']
  # check for PM within entire note
  pmAvail <- 'pm' %in% f
  if(!pmAvail) return(xx)
  # apply to mention level
  # first row should always fail
  isna <- which(is.na(f) & xx[,'drugname_start'] == c(Inf, xx[-1,'drugname_start']))
  if(length(isna)) {
    ix <- which(f[isna-1] %in% c('am','noon'))
    xx[isna[ix], 'freq'] <- 'pm'
  }
  xx
}

# borrow unique values
borrowVal <- function(xx, col, elig) {
  f <- xx[,col]
  isna <- is.na(f)
  if(!missing(elig)) {
    isna <- isna & elig
  }
  if(!any(isna)) return(xx)
  fv <- unique(f[!isna])
  if(length(fv) == 1) {
# not sure which is faster
    f[isna] <- fv
    xx[[col]] <- f
#     xx[isna, col] <- fv
  }
  xx
}

reOrder <- function(xx, key = 'rowOrder') {
  xx <- xx[order(xx[[key]]),]
  rownames(xx) <- NULL
  xx
}

borrowWithinDoseSeq <- function(x, value) {
  # a dose.seq starts with 1 then continues
  dseq <- x[,'dose.seq']
  # every non-dose.seq needs unique day index
  dseq[is.na(dseq)] <- 1
  # create unique day index for each group of dose.seq
  dayix <- unlist(tapply(dseq, x[,'key1'], function(i) cumsum(i == 1)))
  x[,'key3'] <- paste(x[,'key1'], dayix, sep = '|')
  vkey <- unique(x[!is.na(x[[value]]),'key3'])
  chkv <- x[,'key3'] %in% vkey
  x1 <- x[chkv,]
  x2 <- x[!chkv,]
  x3 <- do.call(qrbind, lapply(split(x1, x1[,'key3']), borrowVal, value))
  x4 <- rbind(x3, x2)
  x4[,'key3'] <- NULL
  reOrder(x4)
}

calcDailyDose <- function(x, useDC = FALSE) {
  ix <- x[,'freq'] %in% c('am','pm','noon')
  x[ix,'intaketime'] <- x[ix,'freq']
  daykey <- unique(x[ix, 'key1'])
  useday <- x[,'key1'] %in% daykey
  x1 <- x[useday,]
  x2 <- x[!useday,]
  if(nrow(x2) > 0) {
    x2[,'dose.seq'] <- NA_real_
    x2[,'dose.daily'] <- x2[,'dose.intake'] * x2[,'freq.num']
  }
  if(nrow(x1) > 0) {
    # actually using rbind.matrix so qrbind unavailable
    daytot <- do.call(rbind, lapply(split(x1, x1[,'key1']), pairDay))
    x1[,'dose.seq'] <- daytot[,1]
    x1[,'dose.daily'] <- daytot[,2]
    # after pairing, if useDC then borrow dosechange
    if(useDC) {
      x1 <- borrowWithinDoseSeq(x1, 'dosechange')
    }
  }
  x <- rbind(x1, x2)
  reOrder(x)
}

daySeqDups <- function(x, key, matchingCols) {
  do.call(qrbind, lapply(split(x, key), function(i) {
    sect <- cumsum(i[,'dose.seq'] == 1)
    # matchingCols: c('dose.intake','dose.daily','dose.seq','intaketime')
    ss <- split(i[,matchingCols], sect)
    pairs <- vapply(ss, function(j) {
      paste(unlist(j), collapse = "|")
    }, character(1))
    ix <- which(duplicated(pairs))
    i[!(sect %in% ix),]
  }))
}

rmDuplicates <- function(x, useRoute = FALSE, useDuration = FALSE, useDoseChange = FALSE, useLastDose = FALSE) {
  useday <- !is.na(x[,'dose.seq'])
  # alternatively, if intaketime -> useday
  x1 <- x[useday,]
  x2 <- x[!useday,]

  # duration could be borrowed if within dose.seq
  if(nrow(x1) > 0 && useDuration) {
    x1 <- borrowWithinDoseSeq(x1, 'duration')
  }

  # additional columns should completely match
  reqMatch <- c('dose.intake','freq.num')
  reqMatchDaySeq <- c('dose.intake','dose.daily','dose.seq','intaketime')
  if(useRoute) {
    reqMatch <- c(reqMatch, 'route')
    reqMatchDaySeq <- c(reqMatchDaySeq, 'route')
  }
  if(useDuration) {
    reqMatch <- c(reqMatch, 'duration')
    reqMatchDaySeq <- c(reqMatchDaySeq, 'duration')
  }
  if(useDoseChange) {
    reqMatch <- c(reqMatch, 'dosechange')
    reqMatchDaySeq <- c(reqMatchDaySeq, 'dosechange')
  }
  if(useLastDose) {
    reqMatch <- c(reqMatch, 'lastdose')
    reqMatchDaySeq <- c(reqMatchDaySeq, 'lastdose')
  }

  # daySeq duplicates must completely match
  key1 <- x1[['key1']]
  key2 <- x1[['key2']]
  x1[,c('key0','key1','key2')] <- NULL
  x1n <- daySeqDups(x1, key1, reqMatchDaySeq)
  x1d <- daySeqDups(x1, key2, reqMatchDaySeq)

  # remove redundant, at intake level
  key1 <- do.call(paste, c(x2[,c('key1',reqMatch)], sep = '|'))
  key2 <- do.call(paste, c(x2[,c('key2',reqMatch)], sep = '|'))
  x2[,c('key0','key1','key2')] <- NULL
  # note level
  xn <- rbind(x2[!duplicated(key1),], x1n)
  # date level
  xd <- rbind(x2[!duplicated(key2),], x1d)
  list(note = xn, date = xd)
}
