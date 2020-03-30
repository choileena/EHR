#' Make Dose Data
#'
#' Takes parsed and paired medication data, calculates dose intake and daily dose, and removes redundant information at the note and date level.
#'
#' This function standardizes frequency, route, and duration entities. Dose amount, strength, and frequency entities are converted to numeric. Rows with only drug name and/or route are removed. If there are drug name changes in adjacent rows, these rows are collpased into one row if there are no conflicts. Missing strengths, dose amounts, frequencies, and routes are borrowed or imputed using various rules. Dose given intake and daily dose are calculated. Redundancies are removed at the date and note level.
#'
#' @param x data.frame
#' @param noteMetaData data.frame
#' @param naFreq Replacing missing frequencies with this value, or the most
#' common.
#'
#' @return A list containing two dataframes, one with the note level and one with the date level collapsed data.
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
#   fns <- strsplit(x[,'filename'], '_')
#   grid <- sapply(fns, `[`, 1)
#   date <- as.Date(sapply(fns, `[`, 2), format = '%Y-%m-%d')
#   note <- sapply(fns, `[`, 3)
  # make keys
  x[,'key0'] <- paste(grid, date, note, x[['drugname_start']], sep = '|')
  x[,'key1'] <- paste(grid, date, note, sep = '|')
  x[,'key2'] <- paste(grid, date, sep = '|')
  # ever a reason to preserver order?
  x[,'rowOrder'] <- order(x[['key1']])

  reqCols <- c('strength.num','doseamt.num','freq')

  xcols <- names(x)
  useRte <- 'route' %in% xcols
  useDur <- 'duration' %in% xcols
  useDC <- 'dosechange' %in% xcols
  useDS <- 'dosestr' %in% xcols

  if(useRte) {
    reqCols <- c(reqCols, 'route')
    rte <- gsub('[. ]', '', tolower(x[,'route']))
    rte[grep('skin', rte)] <- 'transdermal'
    rte[grep('iv|intravenous', rte)] <- 'iv'
    rte[grep('mouth|oral|po', rte)] <- 'orally'
    rte[grep('subcut|sq', rte)] <- 'sq'
    rte[nchar(rte) == 0] <- NA
    x[,'route'] <- rte
    na.route <- most(rte)
    if(is.null(na.route)) {
      warning('unable to calculate mode for route, naRoute set to "orally"')
      na.route <- 'orally'
    }
  }

  if(useDur) {
    reqCols <- c(reqCols, 'duration')
    dur <- gsub(' ', '', tolower(x[,'duration']))
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
    x[,'duration'] <- dur
  }

  if(useDC) {
    reqCols <- c(reqCols, 'dosechange')
    x[!is.na(x[,'dosechange']) & nchar(x[,'dosechange']) == 0,'dosechange'] <- NA
  }

  if(useDS) {
    reqCols <- c(reqCols, 'dosestr.num')
    cstrg <- sub('^([0-9.]+)[^0-9.].*', '\\1', sub('two', '2', tolower(x[,'dosestr'])))
    x[,'dosestr.num'] <- nowarnnum(cstrg)
  }

  cstrg <- sub('two', '2', tolower(x[,'strength']))
  # if STR1/STR2, consider duplicate row with am/pm
  ix <- grep("^[0-9.]+[ ]?/[ ]?[0-9.]+", cstrg)
  # exclude row with non-missing frequency
  ix <- ix[is.na(x[ix, 'freq']) | nchar(x[ix, 'freq']) == 0]
  if(length(ix)) {
    newdat <- x[ix,]
    csa <- sub("^([0-9.]+)[ ]?/[ ]?([0-9.]+)[^0-9.].*", "\\1", cstrg[ix])
    csb <- sub("^([0-9.]+)[ ]?/[ ]?([0-9.]+)[^0-9.].*", "\\2", cstrg[ix])
    cstrg[ix] <- csa
    x[ix,'freq'] <- 'am'
  }
  # if STR1-STR2, consider duplicate row with am/pm or average
  # or maybe just duplicate row
  dash_ix <- grep("^[0-9.]+[ ]?-[ ]?[0-9.]+", cstrg)
  if(length(dash_ix)) {
    dash_dat <- x[dash_ix,]
    cs1 <- sub("^([0-9.]+)[ ]?-[ ]?([0-9.]+)[^0-9.].*", "\\1", cstrg[dash_ix])
    cs2 <- sub("^([0-9.]+)[ ]?-[ ]?([0-9.]+)[^0-9.].*", "\\2", cstrg[dash_ix])
    cstrg[dash_ix] <- cs1
#   cstrg[ix] <- sprintf("%.2f", (as.numeric(cs1) + as.numeric(cs2)) / 2)
  }
  cstrg <- sub('^([0-9.]+)[^0-9.].*', '\\1', cstrg)
  x[,'strength.num'] <- nowarnnum(cstrg)
  if(length(ix)) {
    newdat[,'strength.num'] <- nowarnnum(csb)
    newdat[,'freq'] <- 'pm'
    x <- rbind(x, newdat)
  }
  if(length(dash_ix)) {
    # duplicate row may break freq
    # however it creates duplicate rowOrder
    dash_dat[,'strength.num'] <- nowarnnum(cs2)
    x <- rbind(x, dash_dat)
  }
  cdose <- sub('[ ]*(cap|capsule|tablet|tab|pill)[s]?', '', tolower(x[,'dose']))
  cdose <- sub('one', 1, cdose)
  cdose <- sub('two', 2, cdose)
  cdose <- sub('three', 3, cdose)
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
  x[,'doseamt.num'] <- nowarnnum(cdose)
  of <- x[,'freq']
  cfreq <- unique(of)
  cfreq1 <- parseFreq(cfreq)
  ix <- match(of, cfreq)
  if(naFreq == 'most') {
#     na.freq <- cfreq1[match(most(of), cfreq)]
    na.freq <- most(cfreq1[ix])
    # possible there isn't a most-used frequency
    if(is.null(na.freq)) {
      warning('unable to calculate mode for frequency, naFreq set to "bid"')
      na.freq <- 'bid'
    }
  } else {
    na.freq <- parseFreq(naFreq)
  }
  cfreq2 <- freqNum(cfreq1)
  # set freq to NA if freqNum failed
  cfreq1[is.na(cfreq2)] <- NA
  cfreq2[is.na(cfreq2)] <- freqNum(na.freq)
  x[,'freq'] <- cfreq1[ix]

  # remove rows with only drugname and/or route
  verCols <- setdiff(reqCols, 'route')
  nas <- rowSums(is.na(x[,verCols])) == length(verCols)
  x <- x[!nas,]

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
  verCols <- setdiff(reqCols, 'dosestr.num')
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
  chkstr <- x[,'key0'] %in% strkey
  x1 <- x[chkstr,]
  x2 <- x[!chkstr,]
  x1 <- do.call(qrbind, lapply(split(x1, x1[,'key0']), borrowVal, 'strength.num'))
  x <- rbind(x1, x2)
  # remove missing strength
  x <- x[!is.na(x[,'strength.num']),]
  x <- reOrder(x)

  # borrow dose
  # first try unique within mention
  # otherwise, impute 1
  ## note this is done before DOSESTR is added back
  dosekey <- unique(x[is.na(x[,'doseamt.num']),'key0'])
  chkdose <- x[,'key0'] %in% dosekey
  x1 <- x[chkdose,]
  x2 <- x[!chkdose,]
  x1 <- do.call(qrbind, lapply(split(x1, x1[,'key0']), borrowVal, 'doseamt.num'))
  x <- rbind(x1, x2)
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
  nodupx <- x[!duplicated(x[,'rowOrder']),]
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

  l1 <- rmDuplicates(x, useRoute = useRte, useDuration = useDur)
  xn <- l1[['note']]
  xd <- l1[['date']]
  # re-combine
  if(useDC) {
    l2 <- rmDuplicates(dc, useRoute = useRte, useDuration = useDur, useDoseChange = TRUE)
    xn <- rbind(xn, l2[['note']])
    xd <- rbind(xd, l2[['date']])
  }

  xn <- reOrder(xn)
  xd <- reOrder(xd)
  xn[,'rowOrder'] <- NULL
  xd[,'rowOrder'] <- NULL
  list(note = xn, date = xd)
}