#' Build-PK-IV Module
#'
#' This module builds PK data for intravenously (IV) administered medications.
#'
#' @param conc concentration data, the output of \code{\link{run_DrugLevel}}, 
#' a filename (CSV, RData, RDS), or a correctly formatted data.frame
#' @param conc.columns a named list that should specify columns in concentration
#' data; \sQuote{id}, \sQuote{datetime}, \sQuote{druglevel} are required.
#' \sQuote{idvisit} may also be specified; \sQuote{idvisit} can be used when there are multiple visits 
#' (i.e., several occasions) for the same subject. \sQuote{datetime} is date and time for
#' concentration measurement, which can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time
#' separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param dose dose data, the output of \code{\link{run_MedStrI}}, a filename
#' (CSV, RData, RDS), or a correctly formatted data.frame
#' @param dose.columns a named list that should specify columns in dose data;
#' \sQuote{id} is required. \sQuote{infuseDatetime} and \sQuote{infuseDose}
#' should be set if infusion dose data is present. \sQuote{infuseTimeExact}
#' may also be specified for infusion data -- this variable represents an
#' precise time, if for example the \sQuote{infuseDatetime} variable is rounded.
#' \sQuote{bolusDatetime} and \sQuote{bolusDose} should be set if bolus dose
#' data is present. A generic \sQuote{date} variable may be provided, agnostic
#' to either infusion or bolus dosing. \sQuote{gap} and \sQuote{weight} column
#' names may also be set. Any of the date-time variables can be specified as a
#' single date-time variable (infuseDatetime = \sQuote{date_time}) or two variables
#' holding date and time separately (e.g., infuseDatetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param censor censoring information, if available; this will censor concentration
#' and dose data for dates occuring after the censor datetime variable.
#' @param censor.columns a named list that should specify columns in censoring data; \sQuote{id},
#' and \sQuote{datetime} are required. \sQuote{datetime} is the date and time when
#' data should be censored. This can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time separately
#' (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param demo.list demographic information, if available; the output from 
#' \code{\link{run_Demo}} or a correctly formatted data.frame
#' @param demo.columns a named list that should specify columns in demographic data;
#' \sQuote{id} is required. \sQuote{weight} and \sQuote{idvisit}
#' may also be used to specify columns for weight or the unique idvisit. Any other columns
#' present in the demographic data are treated as covariates.
#' @param lab.list lab data, if available; the output from \code{\link{run_Labs}} or 
#' a correctly formatted list
#' @param lab.columns a named list that should specify columns in lab data; \sQuote{id},
#' and \sQuote{datetime} are required. \sQuote{datetime} is the date and time when
#' the lab data was obtained, which can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time separately
#' (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})). Any other columns present in lab
#' data are treated as lab values.
#' @param dosePriorWindow Dose data is merged with drug level data. This value sets the
#' time frame window with the number of days prior to the first drug level data; defaults to 7.
#' @param labPriorWindow Lab data is merged with drug level data. This value sets the
#' time frame window with the number of days prior to the first drug level data; defaults to 7.
#' @param postWindow Data is merged with drug level data. This postWindow can set the end time
#' for the drug level data, being the number of days after the first drug level data. The
#' default (NA) will use the date of the last drug level data.
#' @param pk.vars variables to include in the returned PK data. The variable \sQuote{date}
#' is a special case; when included, it maps the \sQuote{time} offset to its original date-time.
#' Other named variables will be merged from the concentration data set. For example,
#' rather than being separate data sets, labs or demographics may already be present in
#' the concentration data. These columns should be named here.
#' @param drugname drug of interest, included in filename of check files. The default (NULL)
#' will produce filenames without drugname included.
#' @param check.path path to \sQuote{check} directory, where check files are
#' created. The default (NULL) will not produce any check files.
#' @param missdemo_fn filename for checking NA frequency among demographic data
#' @param faildupbol_fn filename for duplicate bolus data
#' @param date.format output format for \sQuote{date} variable
#' @param date.tz output time zone for \sQuote{date} variable
#' @param isStrict logical; when TRUE dose amount totals are strictly summed rather than repeated
#' hourly until stopped
#'
#' @details See EHR Vignette for Structured Data.
#'
#' Regarding the \sQuote{gap} variable in the dose dataset, if \sQuote{gap} is specified in \sQuote{dose.columns}, 
#' it allows a continuous infusion given when there are missing records 
#' between infusion dosing records. For example, suppose that \sQuote{gap} = 60 is defined 
#' (which is typical gap size when infusion dosing is supposed to be recorded hourly for inpatients) 
#' and time between two records (i.e., gap) are greater than 1 hour (i.e., missing records). If the gap 
#' between the two records is less or equal to twice of the gap (i.e., 2*60 = 120 min), a continuous infusion 
#' is assumed until the 2nd dose record; otherwise, the first infusion is assumed to be stopped 
#' (i.e., add zero doses) after 60 min (i.e., equal to the gap size)  and a new infusion (the 2nd record) starts at its recorded time. 
#'
#' @return PK data set
#'
#' @examples 
#' # make fake data
#' set.seed(6543)
#' 
#' build_date <- function(x) format(seq(x, length.out=5, by="1 hour"), "%Y-%m-%d %H:%M")
#' dates <- unlist(lapply(rep(Sys.time(),3), build_date))
#'
#' plconc <- data.frame(mod_id = rep(1:3,each=5),
#'                    mod_id_visit = rep(1:3,each=5)+0.1,
#'                    event = rep(1:5,times=3),
#'                    conc.level = 15*exp(-1*rep(1:5,times=3))+rnorm(15,0,0.1),
#'                    date.time = as.POSIXct(dates))
#'
#' ivdose <- data.frame(mod_id = 1:3,
#'                      date.dose = substr(dates[seq(1,15,by=5)],1,10),
#'                      infuse.time.real = NA, infuse.time = NA, infuse.dose = NA,
#'                      bolus.time = as.POSIXct(dates[seq(1,15,by=5)])-300,
#'                      bolus.dose = 90,
#'                      maxint = 0L,
#'                      weight = 45)
#' 
#' 
#' run_Build_PK_IV(conc = plconc,
#'                 conc.columns = list(id = 'mod_id', datetime = 'date.time',
#'                   druglevel = 'conc.level', idvisit = 'mod_id_visit'),
#'                 dose = ivdose,
#'                 dose.columns = list(id = 'mod_id', date = 'date.dose',
#'                   bolusDatetime = 'bolus.time', bolusDose = 'bolus.dose',
#'                   gap = 'maxint', weight = 'weight'),
#'                 pk.vars = 'date')
#'
#' @export

run_Build_PK_IV <- function(conc, conc.columns = list(),
                            dose, dose.columns = list(),
                            censor = NULL,
                            censor.columns = list(),
                            demo.list = NULL, demo.columns = list(),
                            lab.list = NULL, lab.columns = list(),
                            dosePriorWindow = 7,
                            labPriorWindow = 7,
                            postWindow = NA,
                            pk.vars = NULL, drugname = NULL, check.path = NULL,
                            missdemo_fn='-missing-demo',
                            faildupbol_fn='DuplicateBolus-',
                            date.format="%m/%d/%y %H:%M:%S",
                            date.tz="America/Chicago",
                            isStrict = FALSE
) {
  conc.req <- list(id = NA, datetime = NA, druglevel = NA, idvisit = NULL)
  dose.req <- list(id = NA, date = NULL, infuseDatetime = NULL, infuseTimeExact = NULL, infuseDose = NULL,
    bolusDatetime = NULL, bolusDose = NULL, gap = NULL, weight = NULL)
  censor.req <- list(id = NA, datetime = NA)
  lab.req <- list(id = NA, datetime = NA)
  demo.req <- list(id = NA, datetime = NULL, idvisit = NULL, weight = NULL)

  # standardize conc data
  conc.col <- validateColumns(conc, conc.columns, conc.req)
  if(length(conc.col$datetime) == 2) {
    concDT <- paste(conc[,conc.col$datetime[1]], conc[,conc.col$datetime[2]])
  } else {
    concDT <- conc[,conc.col$datetime]
  }
  conc[,'date.time'] <- pkdata::parse_dates(concDT)
  # find variables to merge in later
  conc.orig <- conc[, setdiff(names(conc), c(conc.col$druglevel, conc.col$idvisit))]
  # require pk.vars to restore?
  orig.vars <- pk.vars[pk.vars %in% names(conc.orig)]
  # ignore variables with these names
  pk.excl <- c('date', 'other', 'multiple.record')
  orig.vars <- setdiff(orig.vars, c(pk.excl, conc.col$id, 'date.time'))
  if(length(orig.vars)) {
    conc.orig <- conc.orig[, c(conc.col$id, 'date.time', orig.vars)]
  } else {
    conc.orig <- NULL
  }
  conc <- conc[,c(conc.col$id, 'date.time', conc.col$druglevel, conc.col$idvisit)]

  # standardize dose data
  dose.col <- validateColumns(dose, dose.columns, dose.req)
  hasInf <- 'infuseDose' %in% names(dose.col)
  hasBol <- 'bolusDose' %in% names(dose.col)
  # force mod_id
  if(dose.col$id != 'mod_id') {
    names(dose)[match(dose.col$id, names(dose))] <- 'mod_id'
  }
  dosecoi <- 'mod_id'
  if(hasInf) {
    if(length(dose.col$infuseDatetime) == 2) {
      infdt <- paste(dose[,dose.col$infuseDatetime[1]], dose[,dose.col$infuseDatetime[2]])
    } else {
      infdt <- dose[,dose.col$infuseDatetime]
    }
    dose[,'infuse.time'] <- pkdata::parse_dates(infdt)
    if(dose.col$infuseDose != 'infuse.dose') {
      dose[,'infuse.dose'] <- dose[,dose.col$infuseDose]
    }
    # require a "real" (accurate) date-time
    if('infuseTimeExact' %in% names(dose.col)) {
      if(length(dose.col$infuseTimeExact) == 2) {
        infdtr <- paste(dose[,dose.col$infuseTimeExact[1]], dose[,dose.col$infuseTimeExact[2]])
      } else {
        infdtr <- dose[,dose.col$infuseTimeExact]
      }
      dose[,'infuse.time.real'] <- pkdata::parse_dates(infdtr)
    } else {
      dose[,'infuse.time.real'] <- dose[,'infuse.time']
    }
    dosecoi <- c(dosecoi, 'infuse.time', 'infuse.time.real', 'infuse.dose')
  }
  if(hasBol) {
    if(length(dose.col$bolusDatetime) == 2) {
      boldt <- paste(dose[,dose.col$bolusDatetime[1]], dose[,dose.col$bolusDatetime[2]])
    } else {
      boldt <- dose[,dose.col$bolusDatetime]
    }
    dose[,'bolus.time'] <- pkdata::parse_dates(boldt)
    if(dose.col$bolusDose != 'bolus.dose') {
      dose[,'bolus.dose'] <- dose[,dose.col$bolusDose]
    }
    dosecoi <- c(dosecoi, 'bolus.time', 'bolus.dose')
  }
  if('date' %in% names(dose.col)) {
    dose[,'date.dose'] <- pkdata::parse_dates(dose[,dose.col$date])
    dosecoi <- c(dosecoi, 'date.dose')
  }
  dosecoi <- c(dosecoi, dose.col$gap, dose.col$weight)
  dose <- dose[,dosecoi]

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
  info <- do.call(pkdata::trimDoses, tdArgs)
  ni <- names(info)
  # require 'date.dose' column
  if(!('date.dose' %in% ni)) {
    tmpDate <- rep(as.Date(NA), nrow(info))
    if(hasInf) {
      tmpDate <- as.Date(info[,'infuse.time'])
    }
    if(hasBol) {
      ix <- which(is.na(tmpDate))
      tmpDate[ix] <- as.Date(info[ix,'bolus.time'])
    }
    info[,'date.dose'] <- tmpDate
  }

  info <- resolveDoseDups_mod(info, checkDir=check.path, drugname=drugname, faildupbol_filename=faildupbol_fn)

  if('gap' %in% names(dose.col)) {
    # force column name 'maxint'
    names(info)[match(dose.col$gap, ni)] <- 'maxint'
    # skip if no infusion data
    if(hasInf) {
      info0 <- addZeroDose(info, infusionDoseTimeVar="infuse.time", infusionDoseVar="infuse.dose",
                          dateVar="date.dose", gapVar='maxint', useNext = FALSE)
    } else {
      info0 <- info
    }
  } else {
    info0 <- info
    info0[,'maxint'] <- 60
  }

  info1 <- info0
  hasDemo <- !is.null(demo.list)
  hasLabs <- !is.null(lab.list)
  if(hasDemo) { # if using demographic data
    demoData <- NULL
    demoExcl <- NULL
    if(inherits(demo.list, 'data.frame')) {
      demoData <- demo.list
    } else {
      if('demo' %in% names(demo.list)) {
        demoData <- demo.list$demo
      }
      if('exclude' %in% names(demo.list)) {
        demoExcl <- demo.list$exclude
      }
    }
    if(is.null(demoData)) {
      warning('Demographic data was provided in an unexpected format and will be ignored')
      hasDemo <- FALSE
    } else {
      # standardize demographic data
      demo.col <- validateColumns(demoData, demo.columns, demo.req)
      # it is unlikely for datetime to be specified
      if('datetime' %in% names(demo.col)) {
        if(length(demo.col$datetime) == 2) {
          demoDT <- paste(demoData[,demo.col$datetime[1]], demoData[,demo.col$datetime[2]])
        } else {
          demoDT <- demoData[,demo.col$datetime]
        }
        # previously, this was ['surgery_date','time_fromor']
        demoData[,'date.time'] <- pkdata::parse_dates(demoDT)
        dem <- demoData[, c(demo.col$id, 'date.time')]
        info1 <- updateInterval_mod(info0, dem)
      }
    }
  }

  # censor if necessary
  if(!is.null(censor)) {
    censData <- read(censor)
    cens.col <- validateColumns(censData, censor.columns, censor.req)
    if(length(cens.col$datetime) == 2) {
      censDT <- paste(censData[,cens.col$datetime[1]], censData[,cens.col$datetime[2]])
    } else {
      censDT <- censData[,cens.col$datetime]
    }
    # previously, this was ['surgery_date','time_fromor']
    censData[,'date.time'] <- pkdata::parse_dates(censDT)
    # censor dose and concentration data at date.time for each ID
    doseCensFlag <- logical(nrow(info1))
    concCensFlag <- logical(nrow(conc))
    doseIx <- tapply(seq_along(doseCensFlag), info1[,'mod_id'], I)
    concIx <- tapply(seq_along(concCensFlag), conc[,conc.col$id], I)
    na_dt <- as.POSIXct(NA)
    for(i in seq(nrow(censData))) {
      id_i <- as.character(censData[i,cens.col$id])
      dt_i <- censData[i,'date.time']
      d_t1 <- d_t2 <- d_t3 <- na_dt
      if(id_i %in% names(doseIx)) {
        if(hasInf) {
          d_t1 <- info1[doseIx[[id_i]], 'infuse.time']
        }
        if(hasBol) {
          d_t2 <- info1[doseIx[[id_i]], 'bolus.time']
        }
        toCens <- (!is.na(d_t1) & d_t1 > dt_i) | (!is.na(d_t2) & d_t2 > dt_i)
        doseCensFlag[doseIx[[id_i]]] <- toCens
      }
      if(id_i %in% names(concIx)) {
        d_t3 <- conc[concIx[[id_i]], 'date.time']
        toCens <- !is.na(d_t3) & d_t3 > dt_i
        concCensFlag[concIx[[id_i]]] <- toCens
      }
    }
    if(any(doseCensFlag)) {
      info1 <- info1[!doseCensFlag,]
    }
    if(any(concCensFlag)) {
      conc <- conc[!concCensFlag,]
    }
  }

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

  pkd <- do.call(rbind, lapply(uids, function(i) {
    datArgs <- list(doseData = doseById[[i]], drugLevelData = drugLevelById[[i]])
    pk <- do.call(pkdata, c(datArgs, pkArgs))
  }))

  if(hasDemo) {
    message(sprintf('The dimension of the PK data before merging with demographics: %s x %s', nrow(pkd), ncol(pkd)))
    message(sprintf('The number of subjects in the PK data before merging with demographics: %s', length(unique(pkd$mod_id))))
  }

  hasMIV <- 'idvisit' %in% names(conc.col)
  if(hasMIV) {
    pkd[,'mod_id_visit'] <- conc[match(pkd[,'mod_id'], conc[,conc.col$id]), conc.col$idvisit]
  } else {
    pkd[,'mod_id_visit'] <- pkd[,'mod_id']
  }

  pk.excl <- setdiff(pk.excl, pk.vars)
  pk.vars <- setdiff(names(pkd), pk.excl)

  if(hasInf && 'weight' %in% names(dose.col)) {
    flow.weight <- info[!is.na(info[,dose.col$weight]), c('mod_id','infuse.time.real',dose.col$weight)]
    # force column name 'weight"
    if(dose.col$weight != 'weight') {
      names(flow.weight)[3] <- 'weight'
    }
    tmp <- merge(pkd, flow.weight, by.x=c('mod_id','date'), by.y=c('mod_id','infuse.time.real'), all.x=TRUE)
  } else {
    tmp <- pkd
    tmp[,'weight'] <- NA_real_
  }

  if(!hasMIV) {
    tmp[,'mod_id_visit'] <- tmp[,'mod_id']
  }

  if(hasLabs) {
    lab.vars <- c()
    # if input is single DF, turn into list
    if(inherits(lab.list, 'data.frame')) {
      lab.list <- list(lab.list)
    }
    for(i in seq_along(lab.list)) {
      lab.col <- validateColumns(lab.list[[i]], lab.columns, lab.req)
      if(length(lab.col$datetime) == 2) {
        labdt <- paste(lab.list[[i]][,lab.col$datetime[1]], lab.list[[i]][,lab.col$datetime[2]])
      } else {
        labdt <- lab.list[[i]][,lab.col$datetime]
      }
      lab.list[[i]][,'date.time'] <- pkdata::parse_dates(labdt)
      cln <- names(lab.list[[i]])
      cln <- setdiff(cln, c(lab.col$id, 'date.time'))
      lab.vars <- c(lab.vars, cln)
      if(length(cln)) {
        curlab <- lab.list[[i]][,c(lab.col$id, 'date.time', cln)]
        labMax <- labPriorWindow * 24
        tmp <- merge_by_time(tmp, curlab, maxTime=labMax, x.id='mod_id', y.id=lab.col$id, x.time='date', y.time='date.time')
      }
    }
    missLab <- setdiff(lab.vars, names(tmp))
    if(length(missLab)) {
      stop(sprintf('there was a problem merging lab variables: %s', paste(missLab, collapse = ', ')))
    }
  } else {
    lab.vars <- NULL
  }

  if(!is.null(conc.orig)) {
    # instead of merge_by_time, could do exact merge on time
    tmp <- merge_by_time(tmp, conc.orig, maxTime = Inf, x.id='mod_id', y.id=conc.col$id, x.time='date', y.time='date.time')
    pk.vars <- c(pk.vars, orig.vars)
  }

  datetime <- as.POSIXct(tmp[,'date'])
  tmp[,'date'] <- format(datetime, format = date.format, tz = date.tz)

  if(hasDemo) {
    if(!('idvisit' %in% names(demo.col))) {
      demoData[,'mod_id_visit'] <- demoData[,demo.col$id]
      demo.col$idvisit <- 'mod_id_visit'
    }
    if('weight' %in% names(demo.col)) {
      names(demoData)[match(demo.col$weight, names(demoData))] <- 'weight'
    }

    tmp <- merge(tmp, demoData, by.x=c('mod_id_visit', 'mod_id'), by.y=c(demo.col$idvisit, demo.col$id), all.x=TRUE)
    if('weight' %in% names(demoData)) {
      ix <- which(is.na(tmp[,'weight.x']))
      tmp[ix,'weight.x'] <- tmp[ix,'weight.y']
      names(tmp)[match(c('weight.x','weight.y'), names(tmp))] <- c('weight','weight_demo')
    }

    # drop mod_id based on exclusion criteria
    message(sprintf('The number of subjects in the demographic file, who meet the exclusion criteria: %s', length(demoExcl)))
    tmp <- tmp[!(tmp[,'mod_id_visit'] %in% demoExcl),]

    #drop if mod_id is missing (i.e. no demographics for this visit)
    tmp <- tmp[!is.na(tmp[,'mod_id']),]
    n_tmp <- names(tmp)

    # check for missing demo
    dd2 <- tmp[tmp$event==0,]

    if(!is.null(check.path)) {
      x <- data.frame(variable = colnames(dd2), freq = colSums(is.na(dd2)))
      x[,'percent'] <- round(x[,'freq'] / nrow(dd2), 2)
      rownames(x) <- NULL
      fn <- file.path(check.path, paste0(drugname, missdemo_fn, '.csv'))
      msg <- sprintf('check NA frequency in demographics, see file %s', fn)
      message(msg)
      write.csv(x, fn, quote=FALSE, row.names=FALSE)
    }

    # return all demo columns
    demo.vars <- setdiff(names(demoData), c(demo.col$idvisit, demo.col$id, demo.col$datetime, 'date.time'))
    # add weight_demo if necessary
    if('weight_demo' %in% n_tmp) {
      demo.vars <- c(demo.vars, 'weight_demo')
    }

    missdemov <- setdiff(demo.vars, n_tmp)
    message(sprintf('Some demographic variables are missing and will be excluded: %s', paste(missdemov, collapse = '\n')))

    demo.vars <- demo.vars[demo.vars %in% n_tmp]
    message(sprintf('The list of final demographic variables: %s', paste(demo.vars, collapse = '\n')))
  } else {
    demo.vars <- NULL
  }

  if(hasLabs) {
    for(i in seq_along(lab.vars)) {
      varLabel <- lab.vars[i]
      missVar <- tmp[is.na(tmp[,varLabel]), 'mod_id_visit']
      if(length(missVar) == 0) {
        msg <- sprintf('Checked: there are no missing %s', varLabel)
      } else {
        msg <- sprintf('List of IDs missing at least 1 %s: %s', varLabel, paste(unique(missVar), collapse = '\n'))
      }
      message(msg)
    }
  }

  misspkv <- setdiff(pk.vars, names(tmp))
  if(length(misspkv)) {
    message(sprintf('Some PK variables are missing and will be excluded: %s', paste(misspkv, collapse = '\n')))
  }

  mainpk <- tmp[, pk.vars, drop = FALSE]
  n_subj <- length(unique(mainpk[['mod_id']]))
  # restore id column name
  idvar <- conc.col$id
  if(hasMIV) {
    mainpk[,'mod_id'] <- NULL
    colord <- c('mod_id_visit', setdiff(names(mainpk), 'mod_id_visit'))
    col1 <- conc.col$idvisit
  } else {
    mainpk[,'mod_id_visit'] <- NULL
    colord <- c('mod_id', setdiff(names(mainpk), 'mod_id'))
    col1 <- idvar
  }
  mainpk <- mainpk[, colord]
  names(mainpk)[1] <- col1

  # rename dose and event
  names(mainpk)[match(c('conc','dose'), names(mainpk))] <- c('dv','amt')
  mainpk[,'event'] <- NULL
  # these should be equivalent
  mainpk[,'mdv'] <- +(is.na(mainpk[,'dv']))
  mainpk[,'evid'] <- +(!is.na(mainpk[,'amt']))

  reqOrder <- c(col1, 'time', 'amt', 'dv', 'rate', 'mdv', 'evid')
  pkOrder <- c(reqOrder, setdiff(names(mainpk), reqOrder))
  tmp3 <- cbind(mainpk[,pkOrder], tmp[, c(demo.vars, lab.vars), drop = FALSE])

  if(hasDemo) {
    msg <- 'The dimension of the final PK data exported with the key demographics: %s x %s with %s distinct subjects (%s)'
  } else {
    msg <- 'The dimension of the final PK data: %s x %s with %s distinct subjects (%s)'
  }
  message(sprintf(msg, nrow(tmp3), ncol(tmp3), n_subj, idvar))
  tmp3
}
