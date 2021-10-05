#' Internal functions for pk analysis
#' 
#' code{takeClosest}: take value at nearest time
#'
#' code{takeClosestTime}: find nearest time
#'
#' code{daysDiff}: number of days between two dates
#'
#' code{minDiff}: difference (in minutes) of time vector
#'
#' code{fixDates}: add leading zeroes to date
#'
#' code{joinFlowMar}: rbind two data sets
#'
#' code{updateInterval_mod}: replace maxint with difference between surgery time
#' and infusion time (if smaller than maxint)
#'
#' code{fix_serum_time}: create time variable
#'
#' code{merge_by_time}: impute in x by closest time in y
#'
#' code{merge_inf_bolus}: combine infusion and bolus into one hourly dose file
#'
#' code{addZeroDose}: add rows of zero dose, after a gap
#'
#' code{pkdata}
#'
#' @name pk-internal
#' @aliases takeClosest takeClosestTime daysDiff minDiff fixDates joinFlowMar
#' updateInterval_mod fix_serum_time merge_by_time merge_inf_bolus
#' addZeroDose pkdata
#' @keywords internal
NULL

takeClosest <- function(time, times, values) {
    dat <- data.frame(times, values)
    dat <- dat[complete.cases(dat),]
    if(nrow(dat) == 0) return(NA)
    dat[[2]][which.min(abs(difftime(time, dat[[1]])))]
}

takeClosestTime <- function(time, times, values) {
  times <- times[!(is.na(times) | is.na(values))]
  if(length(times) == 0) return(NA)
  times[which.min(abs(difftime(time, times)))]
}

daysDiff <- function(date1, date2) {
  d1 <- as.Date(date1, "%m/%d/%Y")
  d2 <- as.Date(date2, "%m/%d/%Y")
  as.numeric(difftime(d2, d1, units = 'days'))
}

# vector of times, return time difference in minutes
minDiff <- function(x) {
    lubridate::as.duration(diff(x)) / lubridate::dminutes(1)
}

# fix the invalid dates here
fixDates <- function(x) {
  m <- sub('^([0-9]{1})/', '0\\1/', x)
  d <- sub('^([0-9]{2}/)([0-9]{1})/', '\\10\\2/', m)
  sub('^([0-9]{2}/[0-9]{2}/)([0-9]{2})(?![0-9])', '\\120\\2', d, perl = TRUE)
}

joinFlowMar <- function(x, y) {
  y[,setdiff(names(x), names(y))] <- NA
  x <- rbind(x, y[,names(x)])
  names(x) <- c('mod_id', 'date.time', 'infuse.dose', 'unit', 'rate', 'weight', 'maxint')
  z <- x[order(x$mod_id, x$date.time),]
  rownames(z) <- NULL
  z
}

updateInterval_mod <- function(inf, demo) {
  # first two columns of demo should be: id|datetime
  # first column of inf should be: id
  demo <- demo[complete.cases(demo),]
  # add leading zeroes
#   date <- fixDates(demo[,2])
#   time <- sub("^([0-9]{2})([0-9]{2})$", "\\1:\\2", sprintf("%04d", demo[,3]))
#   dt <- paste(date, time)
#   surg.time <- pkdata::parse_dates(dt)
  surg.time <- demo[,2]
  dsid <- inf[,1]
  ids <- unique(dsid)
  ids <- ids[!is.na(ids)] # drop if id==NA
  for(i in seq_along(ids)) {
    d.ix0 <- which(ids[i] == demo[,1])
    ix <- which(ids[i] == dsid)

    if(length(d.ix0)) {
      # find closest surg.time to start of infusion
      min.d.ix <- which.min(abs(difftime(surg.time[d.ix0], inf[ix[1],'infuse.time'], units='mins')))
      td <- difftime(surg.time[d.ix0[min.d.ix]], inf[ix,'infuse.time'], units='mins')
      mod <- which(td > 0 & td < inf[ix,'maxint'])
      if(length(mod)) {
        inf[ix[mod], 'maxint'] <- td[mod]
        # print(paste('maxint changed for',ids[i]))
      }
    }
  }
  inf
}

fix_serum_time <- function(y) {
  # add leading zeroes
  date <- fixDates(y[,'date'])
  dt <- paste(date, y[,'time'])
  y[,'date.time'] <- pkdata::parse_dates(dt)
  y
}

merge_by_time <- function(x, y, select=c(), maxTime=168, x.id='id', y.id='study_id', x.time='date', y.time='date.time') {
  colX <- names(x)
  colY <- names(y)
  stopifnot(c(x.id, x.time) %in% colX)
  if(length(select) == 0) {
    select <- setdiff(colY, c(y.id, y.time))
  }
  stopifnot(c(y.id, y.time, select) %in% colY)
  x[,setdiff(select, colX)] <- NA
  # convert to date-time if necessary
  if(!inherits(x[,x.time], 'POSIXt')) {
    x[,x.time] <- pkdata::parse_dates(x[,x.time])
  }
  if(!inherits(y[,y.time], 'POSIXt')) {
    y[,y.time] <- pkdata::parse_dates(y[,y.time])
  }
  # merge by ID and closest date (within maxTime hours)
  uid <- unique(x[,x.id])
  res <- vector('list', length(uid))
  lx <- split(x, x[,x.id])
  ly <- split(y, y[,y.id])
  for(i in seq_along(res)) {
    id <- as.character(uid[i])
    sx <- lx[[id]]
    sy <- ly[[id]]
    if(!is.null(sx) && !is.null(sy)) {
      for(j in select) {
        sz <- sy[!is.na(sy[,j]),]
        if(nrow(sz)) {
          best <- sapply(sx[,x.time], function(i) which.min(abs(i-sz[,y.time])))
          good <- abs(difftime(sx[,x.time], sz[best,y.time], units='hours')) <= maxTime
          missing <- is.na(sx[,j])
          gmix <- good & missing
          sx[gmix,j] <- sz[best[gmix],j]
        }
      }
    }
    res[[i]] <- sx
  }
  do.call(rbind, res)
}

# combine infusion and bolus into one hourly dose file
merge_inf_bolus <- function(inf.info, y) {
  hasInfusion <- nrow(inf.info) > 0
  hasBolus <- nrow(y) > 0
  if(hasInfusion) {
    names(inf.info)[1] <- 'mod_id'
    inf.info$date.dose <- format(inf.info$date.time, "%Y-%m-%d")
    inf.info$infuse.time <- format(inf.info$date.time, "%H:%M")
    inf.info$bolus.time <- NA
    inf.info$bolus.dose <- NA
    inf.info$given.dose <- inf.info$infuse.dose
    inf.info$infuse.dose <- inf.info$rate
  }

  if(hasBolus) {
    names(y) <- c('mod_id', 'date.time', 'bolus.dose')
    bol.info <- y[complete.cases(y),]
    bol.info$date.dose <- format(bol.info$date.time, "%Y-%m-%d")
    bol.info$bolus.time <- format(bol.info$date.time, '%H:%M')
    bol.info$infuse.time <- NA
    bol.info$infuse.dose <- NA
    bol.info$given.dose <- NA
    bol.info$maxint <- 0
    bol.info$weight <- NA
  }

  req.cols <- c('mod_id','date.dose','infuse.time','infuse.dose','given.dose','bolus.time','bolus.dose','date.time','maxint','weight')
  if(hasInfusion && hasBolus) {
    dose.info <- rbind(inf.info[,req.cols], bol.info[,req.cols])
  } else if(hasInfusion) {
    dose.info <- inf.info[,req.cols]
  } else if(hasBolus) {
    dose.info <- bol.info[,req.cols]
  } else {
    jnk <- as.data.frame(matrix(NA, 1, length(req.cols)))
    names(jnk) <- req.cols
    dose.info <- jnk[FALSE,]
  }
  dose.info <- dose.info[order(dose.info$mod_id, dose.info$date.time),]
  rownames(dose.info) <- NULL
  dose.info
}

# add 0-dose values
addZeroDose <- function(doseData, infusionDoseTimeVar=NULL, infusionDoseVar=NULL, dateVar=NULL, gapVar=NULL, useNext = TRUE) {
    columnNames1 <- names(doseData)
    idVar <- 'mod_id'
    idcol <- match(idVar, columnNames1)
    if(is.na(idcol)) stop("an ID column is required")
    if(!is.null(infusionDoseTimeVar) && !(infusionDoseTimeVar %in% columnNames1)) stop(sprintf("column %s does not exist", infusionDoseTimeVar))
    if(!is.null(infusionDoseVar) && !(infusionDoseVar %in% columnNames1))  stop(sprintf("column %s does not exist", infusionDoseVar))
    if(!is.null(dateVar) && !(dateVar %in% columnNames1))  stop(sprintf("column %s does not exist", dateVar))
    if(!is.null(gapVar) && !(gapVar %in% columnNames1))  stop(sprintf("column %s does not exist", gapVar))
    rtname <- sprintf("%s.real", infusionDoseTimeVar)
    rtcol <- match(rtname, columnNames1)
    if(is.null(rtcol)) stop("real time column is not present")
    ocols <- setdiff(columnNames1, c(idVar,dateVar,rtname,infusionDoseTimeVar,gapVar))
    doseData[,'dosetime123'] <- pkdata::parse_dates(doseData[,infusionDoseTimeVar])
    doseData[,'dosetimereal123'] <- pkdata::parse_dates(doseData[,rtcol])
    fmt1 <- pkdata::guessDateFormat(doseData[,dateVar])
    fmt2 <- pkdata::guessDateFormat(doseData[,rtcol])
    fmt3 <- pkdata::guessDateFormat(doseData[,infusionDoseTimeVar])
#     fmt1 <- '%Y-%m-%d'
#     fmt2 <- '%Y-%m-%d %H:%M:%S'
#     fmt3 <- '%Y-%m-%d %H:%M:%S'
    dd <- split(doseData, doseData[,idVar])
    newdose <- vector('list', length(dd))
    for(i in seq_along(newdose)) {
      dose.info <- dd[[i]]
      # ensure ordered by dosetime
      dose.info <- dose.info[order(dose.info[,'dosetimereal123']),]
      gapsize <- as.numeric(diff(dose.info[,'dosetime123']), units='mins')
      realgapsize <- as.numeric(diff(dose.info[,'dosetimereal123']), units='mins')
      if(useNext) {
        rn <- 1
      } else {
        rn <- nrow(dose.info)
      }
      # typically, is gapsize greater than two hours -- and dose isn't zero?
      dose1 <- dose.info[-rn, infusionDoseVar]
      gap1 <- dose.info[-rn, gapVar]*2
      toadd <- which(pmin(gapsize, realgapsize) > gap1 & dose1 != 0)
      if(length(toadd)) {
        if(useNext) toadd <- toadd + 1
        dose.info <- dose.info[toadd,]
        gaptime <- lubridate::dhours(dose.info[,gapVar]/60)
        dose.info[, ocols] <- NA
        t1 <- dose.info[,'dosetime123']
        t2 <- dose.info[,'dosetimereal123']
        if(useNext) {
          t1 <- t1 - gaptime
          t2 <- t2 - gaptime
        } else {
          t1 <- t1 + gaptime
          t2 <- t2 + gaptime
        }
        dose.info[,'dosetime123'] <- t1
        dose.info[,'dosetimereal123'] <- t2
        dose.info[,dateVar] <- format(dose.info[,'dosetimereal123'], fmt1)
        dose.info[,rtcol] <- format(dose.info[,'dosetimereal123'], fmt2)
        dose.info[,infusionDoseTimeVar] <- format(dose.info[,'dosetime123'], fmt3)
        dose.info[,infusionDoseVar] <- 0
        newdose[[i]] <- dose.info
      }
    }
    newdose <- do.call(rbind, newdose)
    allData <- rbind(doseData, newdose)[, columnNames1]
    allData <- allData[order(allData[,idVar], allData[,rtcol]),]
    rownames(allData) <- NULL
    allData
}

# function changes when rate var exists
pkdata <- function(doseData, drugLevelData, doseIdVar = "id",
                    drugLevelTimeVar="date.time", drugLevelVar="fent.level",
                    infusionDoseTimeVar=NULL, infusionDoseVar=NULL, infusionRemove=c(FALSE, TRUE),
                    bolusDoseTimeVar=NULL, bolusDoseVar=NULL, bolusRemove=c(FALSE, TRUE),
                    otherDoseTimeVar=NULL, otherDoseVar=NULL, otherRemove=c(TRUE, FALSE), otherRateVar=NULL,
                    infusionCalcDose=NULL, intervalVar=NULL) {
    columnNames1 <- names(doseData)
    columnNames2 <- names(drugLevelData)
    # relying on doseData and drugLevelData to have ID column
    idcol <- match(doseIdVar, columnNames1)
    if(is.na(idcol)) stop("an ID column is required for each dataset")
    noInfusion <- noBolus <- noOther <- FALSE
    if(is.null(drugLevelTimeVar) || !(drugLevelTimeVar %in% columnNames2)) stop(sprintf("column %s does not exist", drugLevelTimeVar))
    if(is.null(drugLevelVar) || !(drugLevelVar %in% columnNames2)) stop(sprintf("column %s does not exist", drugLevelVar))
    if(!is.null(infusionDoseTimeVar) && !(infusionDoseTimeVar %in% columnNames1)) stop(sprintf("column %s does not exist", infusionDoseTimeVar))
    if(!is.null(bolusDoseTimeVar) && !(bolusDoseTimeVar %in% columnNames1))  stop(sprintf("column %s does not exist", bolusDoseTimeVar))
    if(!is.null(otherDoseTimeVar) && !(otherDoseTimeVar %in% columnNames1))  stop(sprintf("column %s does not exist", otherDoseTimeVar))
    if(!is.null(infusionDoseVar) && !(infusionDoseVar %in% columnNames1))  stop(sprintf("column %s does not exist", infusionDoseVar))
    if(!is.null(bolusDoseVar) && !(bolusDoseVar %in% columnNames1))  stop(sprintf("column %s does not exist", bolusDoseVar))
    if(!is.null(otherDoseVar) && !(otherDoseVar %in% columnNames1))  stop(sprintf("column %s does not exist", otherDoseVar))
    if(!is.null(otherRateVar) && !(otherRateVar %in% columnNames1))  stop(sprintf("column %s does not exist", otherRateVar))
    if(!is.null(infusionCalcDose) && !(infusionCalcDose %in% columnNames1))  stop(sprintf("column %s does not exist", infusionCalcDose))
    if(!is.null(intervalVar) && !(intervalVar %in% columnNames1))  stop(sprintf("column %s does not exist", intervalVar))
    if(is.null(infusionDoseTimeVar) || is.null(infusionDoseVar))  infusionRemove <- noInfusion <- TRUE
    if(is.null(bolusDoseTimeVar) || is.null(bolusDoseVar)) bolusRemove <- noBolus <- TRUE
    if(is.null(otherDoseTimeVar) || is.null(otherDoseVar)) otherRemove <- noOther <- TRUE
    ### infusion/bolus/other-remove isn't used for anything ###
    if(noInfusion && noBolus && noOther) stop("you must have at least one dose and dose time variable")
    x <- doseData
    colcheck <- c()
    if(!noInfusion) colcheck <- append(colcheck, infusionDoseVar)
    if(!noBolus) colcheck <- append(colcheck, bolusDoseVar)
    if(!noOther) colcheck <- append(colcheck, otherDoseVar)
    # return nothing if no data is found
    if(nrow(x) == 0 || !any(x[,colcheck] != 0, na.rm=TRUE)) return()
    dltv <- match(drugLevelTimeVar, columnNames2)
    dlv <- match(drugLevelVar, columnNames2)
    # subset drug level data on non-missing plasma levels - SHOULD I DO THIS?
    y <- drugLevelData[!is.na(drugLevelData[,dlv]),]
    if(nrow(y) == 0) return()
    # normally infusion time has been rounded, so keep track of the original
    if(!noInfusion) {
        rtcol <- match(sprintf("%s.real", infusionDoseTimeVar), columnNames1)
        if(is.null(rtcol)) stop("real time column is not present")
    }
    # given the min of each time variable, determine the first/origin
    # originally, defaulted time to drug level time - we never want this to occur
    # init.time <- min(pkdata::guessDateFormat(y[,dltv], TRUE), na.rm=TRUE)
    init.time <- NA
    if(!noInfusion && !all(is.na(x[,rtcol]))) {
        init.time <- min(pkdata::parse_dates(x[,rtcol]), na.rm=TRUE)
    }
    if(!noBolus && !all(is.na(x[,bolusDoseTimeVar]))) {
        if(all(is.na(init.time))) init.time <- min(pkdata::parse_dates(x[,bolusDoseTimeVar]), na.rm=TRUE)
        else init.time <- append(init.time, min(pkdata::parse_dates(x[,bolusDoseTimeVar]), na.rm=TRUE))
    }
    if(!noOther && !all(is.na(x[,otherDoseTimeVar]))) {
        if(all(is.na(init.time))) init.time <- min(pkdata::parse_dates(x[,otherDoseTimeVar]), na.rm=TRUE)
        else init.time <- append(init.time, min(pkdata::parse_dates(x[,otherDoseTimeVar]), na.rm=TRUE))
    }
    if(all(is.na(init.time))) stop("Unable to find starting date, are all dates NA?")
    init.time <- min(init.time, na.rm=TRUE)
    # use init.time to generate time var (distance from origin) -- but within each step
    x$time <- NA
    infuse.data <- NULL
    infuse.bolus <- NULL
    bolus.data <- NULL
    other.data <- NULL
    # create infusion pk data frame
    if(!noInfusion) {
        idv <- match(infusionDoseVar, columnNames1)
        # irv is length-0 if iCD is NULL
        irv <- match(infusionCalcDose, columnNames1)
        # gapv can specify intervals other than 1 hour
        gapv <- match(intervalVar, columnNames1)
        # subset on non-missing infusion dose and times
        dose.info <- x[!is.na(x[,rtcol]) & !is.na(x[,idv]),]
        nr_di <- nrow(dose.info)
        if(nr_di > 0) {
            real.time <- pkdata::parse_dates(dose.info[,rtcol])
            # find hourly time difference between real.time and init.time
            dose.info$time <- as.numeric(difftime(real.time, init.time), units='hours')
            # re-order by time
            dose.info <- dose.info[order(dose.info$time),]
            # indicator for change in dosage
            changeIndexStart <- c(1, which(abs(diff(dose.info[,idv])) > 0.0001)+1)
            changeIndexEnd <- c(which(abs(diff(dose.info[,idv])) > 0.0001), nr_di)
            changeIndex <- changeIndexStart
            dose.info[changeIndex, 'change'] <- 1
            infuse.data <- data.frame(id=dose.info[changeIndex, idcol], date=real.time[changeIndex],
                time=dose.info$time[changeIndex], rate=dose.info[changeIndex, idv],
                dose=NA, conc=NA, event=1, other=0, skip1=0, skip2=0, skip3=0, skip4more=0, multiple.record=0, stringsAsFactors=FALSE
            )
            if(length(irv)) {
                for(i in seq(nrow(infuse.data))) {
                  infuse.data[i, 'dose'] <- sum(dose.info[seq(changeIndexStart[i], changeIndexEnd[i]), irv])
                }
            } else {
                # calculate dose based on rate times length on dose
                gap <- 1
                if(length(gapv)) {
                  gap <- dose.info[nr_di, gapv]/60
                }
                infuse.data$dose <- infuse.data$rate * diff(c(infuse.data$time, dose.info[nr_di, "time"]+gap))
            }
            # calculate skips/imputed for each range between dose change
            if('skips' %in% columnNames1) {
                nr_id <- nrow(infuse.data)
                for(i in seq(nr_id)) {
                    if(i < nr_id) {
                        tmp <- dose.info[dose.info[,'time'] >= infuse.data$time[i] & dose.info[,'time'] < infuse.data$time[i+1], 'skips']
                    } else {
                        tmp <- dose.info[dose.info[,'time'] >= infuse.data$time[i], 'skips']
                    }
                    # if tmp is empty, we had duplicate time and skip will be zero
                    if(nrow(tmp) > 0) {
                        infuse.data[i, "skip4more"] <- sum(tmp$skips > 1)
                        # tmp is now the actual vector of skip values
                        tmp <- tmp[tmp$skips <= 1, 'skips']
                        # next four lines calculate counts of contiguous skips
                        a <- which(c(tmp[1], diff(tmp) == -1) & tmp==0)
                        b <- which(c(tmp[1], diff(tmp) == 1) & tmp==1)
                        if(length(a) < length(b)) a <- c(a, length(tmp)+1)
                        cnts <- table(a-b)
                        if(1 %in% names(cnts)) infuse.data[i, "skip1"] <- cnts[names(cnts) == 1]
                        if(2 %in% names(cnts)) infuse.data[i, "skip2"] <- cnts[names(cnts) == 2]
                        if(3 %in% names(cnts)) infuse.data[i, "skip3"] <- cnts[names(cnts) == 3]
                    }
                }
            }
            # throw out records with dose of zero
            infuse.data <- infuse.data[infuse.data$dose != 0,]
            # keep records with tobolus set
            tobolus <- which(dose.info$tobolus == 1)
            if(length(tobolus)) {
                # tobolus is created when searching duplicates, thus set multiple.record
                infuse.bolus <- data.frame(id=dose.info[tobolus, idcol], date=dose.info[tobolus, rtcol],
                    time=dose.info$time[tobolus], rate=0, dose=dose.info[tobolus, idv],
                    conc=NA, event=1, other=0, skip1=0, skip2=0, skip3=0, skip4more=0, multiple.record=1, stringsAsFactors=FALSE
                )
                # don't want tobolus already found in infuse.data
                infuse.bolus$dose[tobolus %in% changeIndex] <- 0
                # don't want tobolus if difference in real.time
                infuse.bolus$dose[hour(real.time[tobolus]) != hour(real.time[tobolus-1])] <- 0
                # throw out zero dose records
                infuse.bolus <- infuse.bolus[infuse.bolus$dose != 0,]
            }
        }
    }
    # create bolus pk data frame
    if(!noBolus) {
        bdtv <- match(bolusDoseTimeVar, columnNames1)
        bdv <- match(bolusDoseVar, columnNames1)
        # subset on non-missing bolus dose and times
        dose.info <- x[!is.na(x[,bdtv]) & !is.na(x[,bdv]),]
        if(nrow(dose.info) > 0) {
            # find hourly time difference
            dose.info$time <- as.numeric(difftime(pkdata::parse_dates(dose.info[,bdtv]), init.time), units='hours')
            bolus.data <- data.frame(id=dose.info[, idcol], date=dose.info[, bdtv], time=dose.info$time,
                rate=0, dose=dose.info[, bdv], conc=NA, event=1, other=0, skip1=0, skip2=0, skip3=0, skip4more=0, multiple.record=0, stringsAsFactors=FALSE
            )
        }
        # throw out zero dose records
        bolus.data <- bolus.data[bolus.data$dose != 0,]
    }
    # create other pk data frame
    if(!noOther) {
        odtv <- match(otherDoseTimeVar, columnNames1)
        odv <- match(otherDoseVar, columnNames1)
        orv <- match(otherRateVar, columnNames1)
        # subset on non-missing other dose and times
        dose.info <- x[!is.na(x[,odtv]) & !is.na(x[,odv]),]
        if(nrow(dose.info) > 0) {
            # find hourly time difference
            dose.info$time <- as.numeric(difftime(pkdata::parse_dates(dose.info[,odtv]), init.time), units='hours')
            other.data <- data.frame(id=dose.info[, idcol], date=dose.info[, odtv], time=dose.info$time,
                rate=0, dose=dose.info[, odv], conc=NA, event=1, other=1, skip1=0, skip2=0, skip3=0, skip4more=0, multiple.record=0, stringsAsFactors=FALSE
            )
            # if orv is present, calculate rate
            if(length(orv)) {
                # look for numbers in the otherRateVar column
                rates <- regexpr("[0-9]+", dose.info[,orv])
                other.data$rate[which(rates > -1)] <- as.numeric(regmatches(dose.info[,orv], rates))
            }
        }
        # throw out zero dose records
        other.data <- other.data[other.data$dose != 0,]
    }
    drug.data.dt <- pkdata::parse_dates(y[, dltv])
    # create drug level pk data frame
    # set rate/dose/event to zero; find concentration
    pid <- x[1,idcol]
    drug.data <- data.frame(id=rep(pid, nrow(y)), date=drug.data.dt, time=NA, rate=NA, dose=NA,
        conc=y[, dlv], event=0, other=0, skip1=0, skip2=0, skip3=0, skip4more=0, multiple.record=0, stringsAsFactors=FALSE
    )
    # find hourly time difference
    drug.data$time <- as.numeric(difftime(drug.data.dt, init.time), units='hours')
    # remove drug level data occurring before time zero
    drug.data <- drug.data[drug.data$time >= 0,]
    # create a combined data frame, ordered by date
    combined <- drug.data
    if(!is.null(infuse.data)) combined <- rbind(combined, infuse.data)
    if(!is.null(infuse.bolus)) combined <- rbind(combined, infuse.bolus)
    if(!is.null(bolus.data)) combined <- rbind(combined, bolus.data)
    if(!is.null(other.data)) combined <- rbind(combined, other.data)
    combined <- combined[order(combined$date),]
    # remove skips if unused
    if(!'skips' %in% columnNames1) {
      combined <- combined[,!grepl('skip', names(combined))]
    }
    # round time variable
    combined$time <- round(combined$time, 2)
    # re-label ID column
    names(combined)[1] <- doseIdVar
    # reset row names
    rownames(combined) <- NULL
    combined
}
