#' Make Dose Data
#'
#' Takes parsed and paired medication data, calculates dose intake and daily dose, and 
#' removes redundant information at the note and date level.
#'
#' This function standardizes frequency, route, and duration entities. Dose amount, strength,
#' and frequency entities are converted to numeric. Rows with only drug name and/or route are
#' removed. If there are drug name changes in adjacent rows (e.g., from a generic to brand name),
#' these rows are collapsed into one row if there are no conflicts. Missing strengths, dose
#' amounts, frequencies, and routes are borrowed or imputed using various rules (see McNeer et al.,
#' 2020 for details). Dose given intake and daily dose are calculated. Redundancies are removed at
#' the date and note level. If time of last dose is being used and it is unique within the level of
#' collapsing, it is borrowed across all rows.
#'
#' @param x data.frame containing the output of \code{\link{buildDose}}, or the output
#' of \code{\link{addLastDose}} if last dose information is being incorporated.
#' @param noteMetaData data.frame containing identifying meta data for each
#' note, including patient ID, date of the note, and note ID. Column names
#' should be set to \sQuote{filename}, \sQuote{pid}, \sQuote{date},
#' \sQuote{note}. Date should have format YYYY-MM-DD.
#' @param naFreq Replacing missing frequencies with this value, or by default the most common value across
#' the entire set in \code{x}.
#'
#' @return A list containing two dataframes, one with the note level and one with the date level
#' collapsed data.
#'
#' @examples
#' data(lam_mxr_parsed)
#' data(lam_metadata)
#'
#' lam_build_out <- buildDose(lam_mxr_parsed)
#'
#' lam_collapsed <- makeDose(lam_build_out, lam_metadata)
#' lam_collapsed[[1]] # Note level collapsing
#' lam_collapsed[[2]] # Date level collapsing
#' @export

makeDose <- function(x, noteMetaData, naFreq = 'most') {
  s2f <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = s2f))
  ix <- match(x[,'filename'], noteMetaData[,'filename'])
  if(any(is.na(ix))) stop('ensure that all filenames in `x` are present in noteMetaData')
  grid <- noteMetaData[ix,'pid']
  date <- as.Date(noteMetaData[ix,'date'], format = '%Y-%m-%d')
  note <- noteMetaData[ix,'note']
  # make keys
  x[,'key0'] <- paste(grid, date, note, x[['drugname_start']], sep = '|')
  x[,'key1'] <- paste(grid, date, note, sep = '|')
  x[,'key2'] <- paste(grid, date, sep = '|')
  # ever a reason to preserver order?
  x[,'rowOrder'] <- order(x[['key1']], x[['drugname_start']])

  reqCols <- c('strength.num','doseamt.num','freq')

  xcols <- names(x)
  useRte <- 'route' %in% xcols
  useDur <- 'duration' %in% xcols
  useDC <- 'dosechange' %in% xcols
  useDS <- 'dosestr' %in% xcols
  useLD <- 'lastdose' %in% xcols

  if(useRte) {
    reqCols <- c(reqCols, 'route')
    rte <- stdzRoute(x[,'route'])
    x[,'route'] <- rte
    na.route <- most(rte)
    if(is.null(na.route)) {
      warning('unable to calculate mode for route, naRoute set to "orally"', call. = FALSE)
      na.route <- 'orally'
    }
  }

  if(useDur) {
    reqCols <- c(reqCols, 'duration')
    x[,'duration'] <- stdzDuration(x[,'duration'])
  }

  if(useDC) {
    reqCols <- c(reqCols, 'dosechange')
    x[!is.na(x[,'dosechange']) & nchar(x[,'dosechange']) == 0,'dosechange'] <- NA
  }

  of <- x[,'freq']
  cfreq <- unique(of)
  cfreq1 <- stdzFreq(cfreq)
  ix <- match(of, cfreq)
  if(naFreq == 'most') {
#     na.freq <- cfreq1[match(most(of), cfreq)]
    na.freq <- most(cfreq1[ix])
    # possible there isn't a most-used frequency
    if(is.null(na.freq)) {
      warning('unable to calculate mode for frequency, naFreq set to "bid"', call. = FALSE)
      na.freq <- 'bid'
    }
  } else {
    na.freq <- stdzFreq(naFreq)
  }
  cfreq2 <- freqNum(cfreq1)
  # set freq to NA if freqNum failed
  cfreq1[is.na(cfreq2)] <- NA
  cfreq2[is.na(cfreq2)] <- freqNum(na.freq)
  x[,'freq'] <- cfreq1[ix]

  # ignore duplicate dash data, by storing rowOrder in vector
  ignDupDashDatDS <- numeric(0)
  if(useDS) {
    reqCols <- c(reqCols, 'dosestr.num')
    cstrg <- stdzStrength(x[,'dosestr'], x[,'freq'])
    x[,'dosestr.num'] <- c(cstrg)
    # add additional rows if necessary
    if('addl_data' %in% names(attributes(cstrg))) {
      addl <- attr(cstrg, 'addl_data')
      ix <- which(lengths(addl) > 0)
      added <- vector('list', length(ix))
      ignDupDashDatDS <- numeric(length(ix))
      for(i in seq_along(ix)) {
        impdat <- addl[[ix[i]]]
        newdat <- x[rep(ix[i], nrow(impdat)),]
        newdat[,'dosestr.num'] <- impdat[,'str']
        if('freq' %in% names(impdat)) {
          newdat[,'freq'] <- impdat[,'freq']
        } else {
          ignDupDashDatDS[i] <- x[ix[i],'rowOrder']
        }
        added[[i]] <- newdat
      }
      # remove original row and add new rows
      x <- rbind(x[-ix,], do.call(rbind, added))
      x <- reOrder(x)
      ignDupDashDatDS <- ignDupDashDatDS[ignDupDashDatDS > 0]
    }
  }

  if(useLD) {
    reqCols <- c(reqCols, 'lastdose')
    x[!is.na(x[,'lastdose']) & nchar(x[,'lastdose']) == 0,'lastdose'] <- NA
  }

  cstrg <- stdzStrength(x[,'strength'], x[,'freq'])
  x[,'strength.num'] <- c(cstrg)
  # ignore duplicate dash data, by storing rowOrder in vector
  ignDupDashDat <- numeric(0)
  # add additional rows if necessary
  if('addl_data' %in% names(attributes(cstrg))) {
    addl <- attr(cstrg, 'addl_data')
    ix <- which(lengths(addl) > 0)
    added <- vector('list', length(ix))
    ignDupDashDat <- numeric(length(ix))
    for(i in seq_along(ix)) {
      impdat <- addl[[ix[i]]]
      newdat <- x[rep(ix[i], nrow(impdat)),]
      newdat[,'strength.num'] <- impdat[,'str']
      if('freq' %in% names(impdat)) {
        newdat[,'freq'] <- impdat[,'freq']
      } else {
        ignDupDashDat[i] <- x[ix[i],'rowOrder']
      }
      added[[i]] <- newdat
    }
    # remove original row and add new rows
    x <- rbind(x[-ix,], do.call(rbind, added))
    x <- reOrder(x)
    ignDupDashDat <- ignDupDashDat[ignDupDashDat > 0]
  }
  ignDupDashDat <- sort(unique(c(ignDupDashDatDS, ignDupDashDat)))
  x[,'doseamt.num'] <- stdzDose(x[,'dose'])

  # remove rows with only drugname and/or route
  verCols <- setdiff(reqCols, 'route')
  nas <- rowSums(is.na(x[,verCols])) == length(verCols)
  x <- x[!nas,]

  # borrow last dose
  # at note level, then date level if unique
  if(useLD) {
    vkey <- unique(x[!is.na(x[,'lastdose']),'key1'])
    chkv <- x[,'key1'] %in% vkey
    if(any(chkv)) {
      x1 <- x[chkv,]
      x2 <- x[!chkv,]
      # use first lastdose
      ldvals <- tapply(x1[,'lastdose'], x1[,'key1'], function(i) i[!is.na(i)][1])
      x1[,'lastdose'] <- ldvals[match(x1[,'key1'], names(ldvals))]
      x <- rbind(x1, x2)
      ldkey <- unique(x[is.na(x[,'lastdose']),'key2'])
      chkld <- x[,'key2'] %in% ldkey
      x1 <- x[chkld,]
      x2 <- x[!chkld,]
      # borrow if unique
      x1 <- do.call(qrbind, lapply(split(x1, x1[,'key2']), borrowVal, 'lastdose'))
      x <- rbind(x1, x2)
      # convert back to POSIXct
      x[,'lastdose'] <- as.POSIXct(as.numeric(x[,'lastdose']), origin = '1970-01-01')
    }
    x <- reOrder(x)
  }

  # borrow dosestr.num - require unique w/in mention
  if(useDS) {
    dskey <- unique(x[is.na(x[,'strength.num']) & is.na(x[,'dosestr.num']) & is.na(x[,'doseamt.num']),'key0'])
    chkds <- x[,'key0'] %in% dskey
    x1 <- x[chkds,]
    x2 <- x[!chkds,]
    x1 <- do.call(qrbind, lapply(split(x1, x1[,'key0']), function(xs) {
      borrowVal(xs, col = 'dosestr.num', elig = is.na(xs[,'strength.num']) & is.na(xs[,'doseamt.num']))
    }))
    x <- rbind(x1, x2)
    x <- reOrder(x)

    hasDS <- !is.na(x[,'dosestr.num'])
    ds <- x[hasDS,]
    x <- x[!hasDS,]
  }

  # find drugname changes
  nr <- nrow(x)
  fn <- tolower(x[,'filename'])
  dn <- tolower(x[,'drugname'])
  verCols <- setdiff(reqCols, c('dosestr.num','lastdose'))
  ix <- which(fn[-1] == fn[-nr] & dn[-1] != dn[-nr])
  if(length(ix)) {
    # first pass-through ignores consecutive merges
    ix1 <- ix[which(c(TRUE, diff(ix) != 1))]
    if(length(ix1)) {
      rowList <- vector('list', length(ix1))
      valList <- rowList
      for(i in seq_along(ix1)) {
        rowix <- seq(ix1[i], length.out=2)
        tmp <- mergeAdjacent(x[rowix,], verCols)
        if(length(tmp)) {
          rowList[[i]] <- rowix
          valList[[i]] <- tmp
        }
      }
      rowI <- unlist(rowList)
      valI <- do.call(qrbind, valList)
      if(length(rowI) > 0L) {
        x[rowI,] <- valI
      }
    }
    # second pass-through allows consecutive merges
    ix2 <- setdiff(ix, ix1)
    for(i in seq_along(ix2)) {
      rowix <- seq(ix2[i], length.out=2)
      tmp <- mergeAdjacent(x[rowix,], verCols)
      if(length(tmp)) {
        # this is really slow
        x[rowix,] <- tmp
      }
    }
  }
  # "bad" strength was tagged for removal
  x <- x[is.na(x[,'strength.num']) | x[,'strength.num'] != -999,]

  if(useDC) {
    hasDC <- !is.na(x[,'dosechange'])
    dc <- x[hasDC,]
    x <- x[!hasDC,]
    # add from dosestr if available
    if(useDS) {
      hasDC <- !is.na(ds[,'dosechange'])
      dc <- rbind(dc, ds[hasDC,])
      ds <- ds[!hasDC,]
    }
  }

  # borrow strength - require unique w/in mention
  strkey <- unique(x[is.na(x[,'strength.num']),'key0'])
  if(length(strkey)) {
    chkstr <- x[,'key0'] %in% strkey
    x1 <- x[chkstr,]
    x2 <- x[!chkstr,]
    x1 <- do.call(qrbind, lapply(split(x1, x1[,'key0']), borrowVal, 'strength.num'))
    x <- rbind(x1, x2)
    # remove missing strength
    x <- x[!is.na(x[,'strength.num']),]
    x <- reOrder(x)
  }

  # borrow dose
  # first try unique within mention
  # otherwise, impute 1
  ## note this is done before DOSESTR is added back
  dosekey <- unique(x[is.na(x[,'doseamt.num']),'key0'])
  if(length(dosekey)) {
    chkdose <- x[,'key0'] %in% dosekey
    x1 <- x[chkdose,]
    x2 <- x[!chkdose,]
    x1 <- do.call(qrbind, lapply(split(x1, x1[,'key0']), borrowVal, 'doseamt.num'))
    x <- rbind(x1, x2)
  }
  ix <- which(is.na(x[,'doseamt.num']))
  x[ix, 'doseamt.num'] <- 1

  # re-combine
  if(useDC) {
    x <- rbind(x, dc)
  }
  if(useDS) {
    x <- rbind(x, ds)
  }
  x <- reOrder(x)

  # borrow freq
  # first check for am/noon/pm dose sequence within note
  # then mode within mention
  # otherwise, impute global mode
#   nodupx <- x[!duplicated(x[,'rowOrder']),]
  # ignore rows created by dash (STR1-STR2)
  nodupx <- x[!(x[,'rowOrder'] %in% ignDupDashDat & duplicated(x[,'rowOrder'])),]
  freq.mention.mode <- tapply(nodupx[,'freq'], nodupx[,'key0'], function(y) {
    yy <- most(y)
    # use global freq if no mode
    if(is.null(yy)) na.freq else yy
  })
  freqkey <- unique(x[is.na(x[,'freq']),'key1'])
  chkfreq <- x[,'key1'] %in% freqkey
  x1 <- x[chkfreq,]
  x2 <- x[!chkfreq,]
  x1 <- do.call(qrbind, lapply(split(x1, x1[,'key1']), borrowFreqDoseSeq))
  x <- reOrder(rbind(x1, x2))
  ix <- which(is.na(x[,'freq']))
  if(length(ix)) {
    # retrieve mode for each mention
    x[ix,'freq'] <- unname(freq.mention.mode[x[ix,'key0']])
  }

  # numeric freq
  x[,'freq.num'] <- cfreq2[match(x[,'freq'], cfreq1)]
  # this is unlikely to occur
  x[is.na(x[,'freq.num']), 'freq.num'] <- freqNum(na.freq)

  # borrow route
  if(useRte) {
    vkey <- unique(x[is.na(x[,'route']),'key1'])
    chkv <- x[,'key1'] %in% vkey
    if(any(chkv)) {
      x1 <- x[chkv,]
      x2 <- x[!chkv,]
      # borrow if unique
      x1 <- do.call(qrbind, lapply(split(x1, x1[,'key1']), borrowVal, 'route'))
      # borrow global mode
      x1[is.na(x1[,'route']),'route'] <- na.route
      x <- rbind(x1, x2)
    }
    x <- reOrder(x)
  }

  # dose intake
  x[,'dose.intake'] <- x[,'strength.num'] * x[,'doseamt.num']
  if(useDS) {
    ix <- which(!is.na(x[,'dosestr.num']) & !is.na(x[,'dose.intake']))
    if(length(ix)) {
      stop('dose.intake should be missing if dosestr is present')
    }
    ix <- which(!is.na(x[,'dosestr.num']) & is.na(x[,'dose.intake']))
    if(length(ix)) {
      x[ix,'dose.intake'] <- x[ix,'dosestr.num']
    }
  }

  # dosechange can be borrowed from empty rows
  if(useDC) {
    nr <- nrow(x)
    fn <- tolower(x[,'filename'])
    dn <- tolower(x[,'drugname'])
    dnchange <- c(fn[-1] == fn[-nr] & dn[-1] != dn[-nr], FALSE)
    ix <- which(dnchange & is.na(x[,'dose.intake']) & !is.na(x[,'dosechange']) & c(is.na(x[-1,'dosechange']), FALSE))
    if(length(ix)) {
      dckey <- x[ix+1,'key0']
      dcval <- x[ix,'dosechange']
      dcix <- match(x[,'key0'], dckey)
      kix <- which(!is.na(dcix))
      x[kix, 'dosechange'] <- dcval[dcix[kix]]
    }
  }
  # at this point, dose.intake is required
  x <- x[!is.na(x[,'dose.intake']),]

  # exclude dosechange again
  if(useDC) {
    # daily dose
    x <- calcDailyDose(x, useDC = TRUE)
    hasDC <- !is.na(x[,'dosechange'])
    dc <- x[hasDC,]
    x <- x[!hasDC,]
  } else {
    # daily dose
    # creates intaketime|dose.seq|dose.daily
    x <- calcDailyDose(x)
  }

  l1 <- rmDuplicates(x, useRoute = useRte, useDuration = useDur, useLastDose = useLD)
  xn <- l1[['note']]
  xd <- l1[['date']]
  # re-combine
  if(useDC) {
    l2 <- rmDuplicates(dc, useRoute = useRte, useDuration = useDur, useDoseChange = TRUE, useLastDose = useLD)
    xn <- rbind(xn, l2[['note']])
    xd <- rbind(xd, l2[['date']])
  }

  xn <- reOrder(xn)
  xd <- reOrder(xd)
  xn[,'rowOrder'] <- NULL
  xd[,'rowOrder'] <- NULL
  list(note = xn, date = xd)
}
