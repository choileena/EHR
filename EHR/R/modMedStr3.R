#' Run Str Data III
#'
#' This module will load and modify structured intravenous (IV) medication data. It expects actual
#' dose amount given. If duration is provided, rate will be calculated.
#'
#' @param dose.path filename of dose data (CSV, RData, RDS), or data.frame
#' @param dose.columns a named list that should specify columns in dose data; \sQuote{id},
#' \sQuote{datetime} and \sQuote{dose} are required.
#' \sQuote{datetime} is date and time for data measurement, which can refer to a single
#' date-time variable (datetime = \sQuote{date_time}) or two variables holding date and
#' time separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})). \sQuote{duration} is optional
#' and should be given in seconds (e.g., 3600 would equate to 1 hour) when present.
#' @param req.vals a named list that can be used to filter data. As an example,
#' req.vals(unit = \sQuote{mg}, route = \sQuote{iv}) would indicate the dose data has a \sQuote{unit}
#' and \sQuote{route} column. In this case any observation with unit != mg or any route != iv
#' would be excluded.
#' @param check.path path to \sQuote{check} directory, where check files are
#' created. The default (NULL) will not produce any check files.
#' @param drugname drug of interest, included in filename of check files. The default (NULL)
#' will produce filenames without drugname included.
#' @param conflict.window Time window (in minutes) to consider nearby doses. The default is 60 minutes.
#' Equal doses within the window will be considered duplicats and removed. Unequal doses
#' will be also be excluded and saved to a check file (if \sQuote{check.path} is set).
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return structured data set
#'
#' @examples
#' # dose data for single user
#' doseData <- structure(list(id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
#'   dt = c("2020-01-01 08:00", "2020-01-02 08:00", "2020-01-02 20:00",
#'     "2020-01-02 20:00", "2020-01-03 08:00", "2020-01-05 08:00", "2020-01-05 08:00",
#'     "2020-01-05 08:00", "2020-01-05 20:00", "2020-01-06 08:00", "2020-01-06 08:00",
#'     "2020-01-06 20:00", "2020-01-07 20:00", "2020-01-07 20:00"),
#'   dose = c(30, 10, 20, 30, 10, 10, 20, 30, 10, 20, 10, 10, 20, 20),
#'   dur = c(3600, 7200, 1800, 3600, 3600, 7200, 1800, 3600, 7200, 1800, 1800, 1800, 1800, 3600)),
#'   class = "data.frame", row.names = c(NA, -14L))
#' 
#' run_MedStrIII(dose.path = doseData,
#'   dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose', duration = 'dur'))
#'
#' @export

run_MedStrIII <- function(dose.path, dose.columns = list(), req.vals = list(), check.path = NULL, drugname = NULL, conflict.window = 60) {
  dose.in <- read(dose.path)
  dose.req <- list(id = NA, datetime = NA, dose = NA, duration = NULL)
  dose.col <- validateColumns(dose.in, dose.columns, dose.req)
  dose.idCol <- dose.col$id
  dose.datetimeCol <- dose.col$datetime
  dose.doseCol <- dose.col$dose
  dose.durCol <- dose.col$duration

  if(length(req.vals) > 0L) {
    req.names <- names(req.vals)
    misscols <- setdiff(req.names, names(dose.in))
    if(length(misscols)) {
      msg <- 'named elements of req.vals should be column names in dose data'
      stop(sprintf('%s; the following are missing: [%s]', msg, paste(misscols, collapse = ',')))
    }
    for(i in seq_along(req.vals)) {
      # val is data from current column
      val <- dose.in[,req.names[i]]
      # it should match a required value
      dose.in <- dose.in[val %in% req.vals[[i]],]
    }
    if(nrow(dose.in) == 0) stop('after restricting to required values, no rows remain')
  }
  doseData <- dose.in
  rm(dose.in)

  if(is.null(dose.durCol)) {
    # rate is actual dose if no duration
    doseData[,'rate'] <- doseData[,dose.doseCol]
  } else {
    doseData[,'rate'] <- 3600 / doseData[,dose.durCol] * doseData[,dose.doseCol]
  }
  c1 <- doseData[,dose.idCol]
  c2 <- pkdata::parse_dates(doseData[,dose.datetimeCol])
  c3 <- doseData[,dose.doseCol]
  c4 <- doseData[,'rate']
  rm(doseData)
  dat <- data.frame(id = c1, date.time = c2, dose = c3, rate = c4)[order(c1, c2),]
  nr <- nrow(dat)

  # remove duplicates
  # use 1-hour window for 'rate'
  window_size <- conflict.window
  if(!is.na(window_size)) {
    timediff <- c(NA, as.numeric(difftime(dat[-1,'date.time'], dat[-nr,'date.time'], unit = 'min')))
    persdiff <- !duplicated(dat[,'id'])
    timediff[persdiff] <- NA
    zz1 <- !is.na(timediff) & timediff < window_size
    zz2 <- which(zz1)
    zz3 <- unique(sort(c(zz2, zz2 - 1)))
    zz4 <- logical(nr)
    zz4[zz3] <- TRUE
    zz4[zz1] <- FALSE
    zz5 <- cumsum(zz4)
    zz5[setdiff(seq(nr), zz3)] <- NA
    dat[,'dupgrp'] <- zz5
    inGrp <- !is.na(dat[,'dupgrp'])
    dg1 <- dat[inGrp,]
    dg2 <- dat[!inGrp,]
    ldz <- split(dg1, dg1[,'dupgrp'])
    ldzu <- lapply(ldz, function(li) {
      # no discrepancy -- collapse
      if(length(unique(li[,'rate'])) == 1) {
        li <- li[1,]
        li[,'dupgrp'] <- NA
      }
      li
    })
    dat <- rbind(dg2, do.call(rbind, ldzu))
    message(sprintf('The number of rows in the original data           %8d
The number of rows after removing the duplicates  %8d', nr, nrow(dat)))
  } else {
    nodup <- !duplicated(do.call(paste, c(dat[,c('id','date.time','rate')], sep = '|')))
    n2 <- sum(nodup)
    message(sprintf('The number of rows in the original data           %8d
The number of rows after removing the duplicates  %8d', nr, n2))
    if(nr > n2) dat <- dat[nodup,]
  }

  dnfn <- ''
  if(!is.null(drugname)) dnfn <- paste0('-', drugname)
  fn <- file.path(check.path, sprintf('fail-rateConflict%s.csv', dnfn))
  # conflicting doses
  if('dupgrp' %in% names(dat)) {
    rnums <- !is.na(dat[,'dupgrp'])
    dat[,'dupgrp'] <- NULL
  } else {
    key <- do.call(paste, c(dat[,c('id','date.time')], sep = '|'))
    repflow <- unique(key[duplicated(key)])
    rnums <- key %in% repflow
  }
  nConf <- sum(rnums)
  if(nConf > 0) {
    message(sprintf('%s conflicting doses have been removed', nConf))
    # has conflict
    dd <- cbind(dat[rnums,], flag = 'exclude')
    # no conflict
    dat <- dat[!rnums,]
    if(!is.null(check.path)) {
      fixfn <- sub('fail', 'fix', fn)
      msg <- sprintf('review file %s AND create %s', fn, fixfn)
      writeCheckData(dd, fn, msg)
      if(file.access(fixfn, 4) != -1) {
        hasfix <- read(fixfn)
        toKeep <- hasfix[,'flag'] != 'exclude'
        nFixed <- sum(toKeep)
        if(nFixed > 0) {
          dat <- rbind(dat, dtMirror(hasfix[toKeep,], dat))
          message(sprintf('file %s read, %s records added', fixfn, nFixed))
        }
      }
    }
  } else {
    message('no conflicting doses found')
  }

  # re-order and re-name
  out <- dat[order(dat[[1]], dat[[2]]),]
  names(out)[1] <- dose.idCol
  rownames(out) <- NULL
  out
}
