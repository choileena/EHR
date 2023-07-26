#' Build-PK Module
#'
#' This module builds PK data for properly formatted dose and concentration data.
#'
#' @param dose dose data, a filename (CSV, RData, RDS), or a correctly formatted data.frame
#' @param dose.columns a named list that should specify columns in dose data;
#' \sQuote{id} is required. Dose data can be provided with the \sQuote{datetime},
#' \sQuote{dose}, and \sQuote{rate} variables. The date-time variable can be specified as a
#' single date-time variable (datetime = \sQuote{date_time}) or two variables
#' holding date and time separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' \sQuote{druglevel} should be present if a separate concentration data set is not provided.
#' @param conc concentration data, a filename (CSV, RData, RDS), or a
#' correctly formatted data.frame. If NULL, concentration data should be present in the dose data.
#' @param conc.columns a named list that should specify columns in concentration
#' data; \sQuote{id}, \sQuote{datetime}, \sQuote{druglevel} are required.
#' @param doseFreq Provides dose schedule interval for repeated dosing case. A value of 2 would
#' indicate two doses per day. The default (NULL) will not use repeated dosing.
#' Alternatively \sQuote{II} can be specified as a dose.column. II=12 would
#' indicate two doses per day.
#' @param dosePriorWindow Dose data is merged with drug level data. This value sets the
#' time frame window with the number of days prior to the first drug level data; defaults to 7.
#' @param postWindow Data is merged with drug level data. This postWindow can set the end time
#' for the drug level data, being the number of days after the first drug level data. The
#' default (NA) will use the date of the last drug level data.
#' @param date.format output format for \sQuote{date} variable
#' @param tz time zone for date-time variables
#'
#' @return PK data set
#'
#' @export

run_Build_PK <- function(dose, dose.columns = list(),
    conc = NULL, conc.columns = list(),
    doseFreq = NULL,
    dosePriorWindow = 7, postWindow = NA,
    date.format="%m/%d/%y %H:%M:%S", tz = ''
) {
  dose.req <- list(id = NA, datetime = NA, dose = NA, rate = NULL, II = NULL)
  hasConc <- !is.null(conc)
  if(hasConc) {
    conc.req <- list(id = NA, datetime = NA, druglevel = NA)
    conc.col <- validateColumns(conc, conc.columns, conc.req)
  } else {
    # if conc is in dose file, it is required
    dose.req <- c(dose.req, druglevel = NA)
  }
  dose.col <- validateColumns(dose, dose.columns, dose.req)
  dcnames <- names(dose.col)
  useRate <- 'rate' %in% dcnames
  useII <- 'II' %in% dcnames

  if(length(dose.col$datetime) == 2) {
    doseDT <- paste(dose[,dose.col$datetime[1]], dose[,dose.col$datetime[2]])
  } else {
    doseDT <- dose[,dose.col$datetime]
  }
  dose[,'date.time'] <- pkdata::parse_dates(doseDT, tz)

  if(hasConc) {
    if(length(conc.col$datetime) == 2) {
      concDT <- paste(conc[,conc.col$datetime[1]], conc[,conc.col$datetime[2]])
    } else {
      concDT <- conc[,conc.col$datetime]
    }
    conc[,'date.time'] <- pkdata::parse_dates(concDT, tz)
  }

  # repeated dosing
  useRD <- !is.null(doseFreq)
  if(useRD) {
    dosing_interval <- 24 / doseFreq # if 2, then 24/2=12 hours
  }
  if(useII) {
    useRD <- TRUE
    dosing_interval <- dose[,dose.col$II]
    dosing_interval <- unique(dosing_interval[!is.na(dosing_interval)])
    # can this be more than one value (either globally or individually)?
    stopifnot(length(dosing_interval) == 1)
  }

  # separate dose from conc if same file
  if(!hasConc) {
    concRow <- !is.na(dose[,dose.columns$druglevel])
    conc <- dose[concRow,]
    dose <- dose[!concRow,]
    conc.col <- list(id = dose.columns$id, datetime = 'date.time', druglevel = dose.columns$druglevel)
  }

  hasDose <- !is.na(dose[,dose.columns$dose])
  hasRate <- rep(TRUE, nrow(dose)) # default to T even if not present
  if(useRate) {
    hasRate <- !is.na(dose[,dose.columns$rate]) & dose[,dose.columns$rate] > 0
  }

  ix1 <- !hasDose & !hasRate
  ix2 <- hasDose & !hasRate
  ix3 <- hasDose & hasRate
  if(any(ix1)) {
    stop('observations are missing dose amount')
  }
  hasInf <- FALSE # this function won't use "infusion" as it has imputation
  hasBol <- FALSE
  hasOther <- FALSE
  bolusDose <- NULL
  otherDose <- NULL

  if(any(ix2)) {
    hasBol <- TRUE
    bolusDose <- dose[ix2, c(dose.columns$id, 'date.time', dose.columns$dose)]
    names(bolusDose) <- c('mod_id', 'bolus.time', 'bolus.dose')
  }

  if(any(ix3)) {
    hasOther <- TRUE
    otherDose <- dose[ix3, c(dose.columns$id, 'date.time', dose.columns$dose, dose.columns$rate)]
    odnames <- c('mod_id', 'other.time', 'other.dose')
    # add rate if present
    if(useRate) {
      odnames <- c(odnames, 'other.rate')
    }
    names(otherDose) <- odnames
  }
  dose <- rbind(bolusDose, otherDose)

  # trim Doses - determine whether each dose is valid by comparing to concentration data
  tdArgs <- list(doseData=dose, drugLevelData=conc, drugLevelID=conc.col$id,
    drugLevelTimeVar="date.time", drugLevelVar=conc.col$druglevel,
    otherDoseTimeVar=NULL, otherDoseVar=NULL, lookForward=dosePriorWindow, last=postWindow
  )
  if(hasInf) {
    tdArgs$infusionDoseTimeVar <- 'infuse.time'
    tdArgs$infusionDoseVar <- 'infuse.dose'
  }
  if(hasBol) {
    tdArgs$bolusDoseTimeVar <- 'bolus.time'
    tdArgs$bolusDoseVar <- 'bolus.dose'
  }
  if(hasOther) {
    tdArgs$otherDoseTimeVar <- 'other.time'
    tdArgs$otherDoseVar <- 'other.dose'
  }
  info <- do.call(pkdata::trimDoses, tdArgs)
  ni <- names(info)
  # require 'date.dose' column
  if(!('date.dose' %in% ni)) {
    tmpDate <- character(nrow(info))
    if(hasInf) {
      tmpDate <- format(info[,'infuse.time'], '%Y-%m-%d')
    }
    if(hasBol) {
      ix <- which(is.na(tmpDate))
      tmpDate[ix] <- format(info[ix,'bolus.time'], '%Y-%m-%d')
    }
    if(hasOther) {
      ix <- which(is.na(tmpDate))
      tmpDate[ix] <- format(info[ix,'other.time'], '%Y-%m-%d')
    }
    info[,'date.dose'] <- as.Date(tmpDate)
  }

  # info <- resolveDoseDups_mod(info, checkDir=NULL)
  # info <- resolveDoseDups_mod(info, checkDir=check.path, drugname=drugname, faildupbol_filename=faildupbol_fn)
  info1 <- info
  info1[,'maxint'] <- 60

  doseById <- split(info1, info1[,'mod_id'])
  drugLevelById <- split(conc, conc[,conc.col$id])
  uids <- as.character(unique(conc[,conc.col$id]))
  # ID needs to be in both data sets
  uids <- uids[uids %in% names(doseById)]
  # default pkdata arguments
  pkArgs <- list(doseIdVar = "mod_id", drugLevelVar=conc.col$druglevel, intervalVar='maxint')
  if(hasInf) {
    pkArgs$infusionDoseTimeVar <- 'infuse.time'
    pkArgs$infusionDoseVar <- 'infuse.dose'
    if(isStrict) {
      pkArgs$infusionCalcDose <- 'infuse.dose'
    }
  }
  if(hasBol) {
    pkArgs$bolusDoseTimeVar <- 'bolus.time'
    pkArgs$bolusDoseVar <- 'bolus.dose'
  }
  if(hasOther) {
    pkArgs$otherDoseTimeVar <- 'other.time'
    pkArgs$otherDoseVar <- 'other.dose'
    if(useRate) {
      pkArgs$otherRateVar <- 'other.rate'
    }
  }

  pkd <- do.call(rbind, lapply(uids, function(i) {
    datArgs <- list(doseData = doseById[[i]], drugLevelData = drugLevelById[[i]])
    pk <- do.call(pkdata, c(datArgs, pkArgs))
  }))
  # if `pkd` is NULL, something bad happened
  if(is.null(pkd)) {
    stop('pk data was not created; either dose or concentration were unavailable')
  }
  # adjust for repeated dosing
  if(useRD) {
    rate_ix <- which(!is.na(pkd[,'rate']))
    t1 <- pkd[rate_ix, 'date']
    # subtract half-an-interval for proper rounding
    t2 <- t1[-1] - 3600 * dosing_interval / 2
    addl_vals <- c(as.numeric(difftime(t2, t1[-length(t1)], units = 'hours')) %/% dosing_interval, 0)
    pkd[rate_ix,'addl'] <- addl_vals
    pkd[rate_ix,'II'] <- dosing_interval

    rd_dat <- pkd[rate_ix,]
    rd_n <- nrow(rd_dat)
    # collapse on same user/dose/rate
    sameuser <- rd_dat[-1,'mod_id'] == rd_dat[-rd_n,'mod_id']
    samedose <- rd_dat[-1,'dose'] == rd_dat[-rd_n,'dose']
    samerate <- rd_dat[-1,'rate'] == rd_dat[-rd_n,'rate']
    sameuserdose <- c(FALSE, sameuser & samedose & samerate)
    if(any(sameuserdose)) {
      rd_torm <- rate_ix[sameuserdose]
      ix_tokp <- setdiff(rate_ix, rd_torm)
      # add back collapsed values (including time at dose) to addl_vals
      rm2kp <- vapply(rd_torm, function(i) max(which(i > ix_tokp)), numeric(1))
      keep_ix <- match(ix_tokp[rm2kp], rate_ix)
      addl_ix <- match(rd_torm, rate_ix)
      rm4addl <- addl_vals[addl_ix] + 1
      for(i in seq_along(rd_torm)) {
        addl_vals[keep_ix[i]] <- addl_vals[keep_ix[i]] + rm4addl[i]
      }
      # update addl
      pkd[ix_tokp,'addl'] <- addl_vals[-addl_ix]
      # update pkd
      pkd <- pkd[-rd_torm,]
    }

    # repeat `addl` calculation and collapsing for dose with no rate
    ## verify rate is NA if not part of input file
    dose_ix <- which(!is.na(pkd[,'dose']) & is.na(pkd[,'rate']))
    if(length(dose_ix) > 0L) {
      t1 <- pkd[dose_ix, 'date']
      # subtract half-an-interval for proper rounding
      t2 <- t1[-1] - 3600 * dosing_interval / 2
      addl_vals <- c(as.numeric(difftime(t2, t1[-length(t1)], units = 'hours')) %/% dosing_interval, 0)
      pkd[dose_ix,'addl'] <- addl_vals
      pkd[dose_ix,'II'] <- dosing_interval

      rd_dat <- pkd[dose_ix,]
      rd_n <- nrow(rd_dat)
      # collapse on same user/dose
      sameuser <- rd_dat[-1,'mod_id'] == rd_dat[-rd_n,'mod_id']
      samedose <- rd_dat[-1,'dose'] == rd_dat[-rd_n,'dose']
      sameuserdose <- c(FALSE, sameuser & samedose)
      if(any(sameuserdose)) {
        rd_torm <- dose_ix[sameuserdose]
        ix_tokp <- setdiff(dose_ix, rd_torm)
        # add back collapsed values (including time at dose) to addl_vals
        rm2kp <- vapply(rd_torm, function(i) max(which(i > ix_tokp)), numeric(1))
        keep_ix <- match(ix_tokp[rm2kp], dose_ix)
        addl_ix <- match(rd_torm, dose_ix)
        rm4addl <- addl_vals[addl_ix] + 1
        for(i in seq_along(rd_torm)) {
          addl_vals[keep_ix[i]] <- addl_vals[keep_ix[i]] + rm4addl[i]
        }
        # update addl
        pkd[ix_tokp,'addl'] <- addl_vals[-addl_ix]
        # update pkd
        pkd <- pkd[-rd_torm,]
      }
    }

    # in this case, last dose should be imputed until last concentration
    if(useII) {
      id_lookup <- tapply(seq(nrow(pkd)), pkd[,'mod_id'], I)
      for(i in seq_along(id_lookup)) {
        # row numbers for i_th person
        id_ix <- id_lookup[[i]]
        # find last dose
        ldi <- max(which(is.na(pkd[id_ix,'conc'])))
        lddt <- pkd[id_ix[ldi],'date']
        # find last conc
        lci <- max(which(!is.na(pkd[id_ix,'conc'])))
        lcdt <- pkd[id_ix[lci],'date'] - 3600 * dosing_interval / 2
        ld_addl <- as.numeric(difftime(lcdt, lddt, units = 'hours')) %/% dosing_interval
        pkd[id_ix[ldi],'addl'] <- ld_addl
      }
    }
  }

  lab.vars <- NULL
  demo.vars <- NULL

  # should be argument
  pk.vars <- 'date'
  # ignore variables with these names
  pk.excl <- c('date', 'other', 'multiple.record')
  pk.excl <- setdiff(pk.excl, pk.vars)
  pk.vars <- setdiff(names(pkd), pk.excl)

  tmp <- pkd
  tmp[,'weight'] <- NA_real_
  tmp[,'date'] <- format(tmp[,'date'], format = date.format)

  misspkv <- setdiff(pk.vars, names(tmp))
  if(length(misspkv)) {
    message(sprintf('Some PK variables are missing and will be excluded: %s', paste(misspkv, collapse = '\n')))
  }

  mainpk <- tmp[, pk.vars, drop = FALSE]
  n_subj <- length(unique(mainpk[['mod_id']]))
  idvar <- dose.columns$id

  # rename dose and event
  names(mainpk)[match(c('mod_id','conc','dose'), names(mainpk))] <- c(idvar, 'dv', 'amt')
  mainpk[,'event'] <- NULL
  # these should be equivalent
  mainpk[,'mdv'] <- +(is.na(mainpk[,'dv']))
  mainpk[,'evid'] <- +(!is.na(mainpk[,'amt']))

  if(useRD) {
    reqOrder <- c(idvar, 'time', 'amt', 'dv', 'rate', 'addl', 'II', 'mdv', 'evid')
  } else {
    reqOrder <- c(idvar, 'time', 'amt', 'dv', 'rate', 'mdv', 'evid')
  }
  if(!useRate) {
    reqOrder <- setdiff(reqOrder, 'rate')
  }
  pkOrder <- c(reqOrder, setdiff(names(mainpk), reqOrder))
  tmp3 <- mainpk[,pkOrder]
  rownames(tmp3) <- NULL
  msg <- 'The dimension of the final PK data: %s x %s with %s distinct subjects (%s)'
  message(sprintf(msg, nrow(tmp3), ncol(tmp3), n_subj, idvar))
  tmp3
}
