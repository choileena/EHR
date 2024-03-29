#' Run Str Data I
#'
#' This module will load and modify structured intravenous (IV) infusion and 
#' bolus medication data.
#'
#' @param mar.path filename of MAR data (CSV, RData, RDS), or data.frame
#' @param mar.columns a named list that should specify columns in MAR data; \sQuote{id},
#' \sQuote{datetime} and \sQuote{dose} are required. \sQuote{drug}, \sQuote{weight},
#' \sQuote{given} may also be specified. \sQuote{datetime} is date and time for data
#' measurement, which can refer to a single date-time variable (datetime = \sQuote{date_time})
#' or two variables holding date and time separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' \sQuote{dose} can also be given as a single variable or two variables. If given as a single column,
#' the column's values should contain dose and units such as \sQuote{25 mcg}. If given as
#' two column names, the dose column should come before the unit column
#' (e.g., dose = c(\sQuote{doseamt}, \sQuote{unit})). \sQuote{drug} can provide list of acceptable drug names.
#' If \sQuote{drug} is present, the \sQuote{medchk.path} argument should also be provided.
#' The \sQuote{given} is a variable that flags whether the medication (inpatient) was given. When it is given, 
#' values shoule be \dQuote{Given}; should be used in conjunction with the \sQuote{medGivenReq} argument.
#' @param medGivenReq if TRUE, values in \sQuote{given} column in MAR data should equal \dQuote{Given}; 
#' if this is FALSE (the default), NA values are also acceptable.
#' @param flow.path filename of flow data (CSV, RData, RDS), or data.frame
#' @param flow.columns a named list that should specify columns in flow data; \sQuote{id},
#' \sQuote{datetime}, \sQuote{finalunits}, \sQuote{unit}, \sQuote{rate}, \sQuote{weight}
#' are required. \sQuote{idvisit} may also be specified. \sQuote{datetime} is date and time
#' for data measurement, which can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time separately
#' (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param medchk.path filename containing data set (CSV, RData, RDS), or data.frame; should have
#' the column \sQuote{medname} with list of acceptable drug names (e.g., brand and generic name, abbreviations)
#' to subset drugs of interest using \sQuote{drug} column in MAR data. This argument can be used when MAR data 
#' contains different drugs that should be excluded. 
#' @param demo.list demographic information; if available, the output from `run_Demo` or 
#' a correctly formatted data.frame, which can be used to impute weight when missing
#' @param demo.columns a named list that should specify columns in demographic data; \sQuote{id},
#' \sQuote{datetime}, and \sQuote{weight} are required. \sQuote{datetime} is the date
#' and time when the demographic data were obtained, which can refer to a single date-time
#' variable (datetime = \sQuote{date_time}) or two variables holding date and time separately
#' (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param missing.wgt.path filename containing additional weight data (CSV, RData, RDS), or data.frame. 
#' The variables in this file should be defined in the \sQuote{wgt.columns} argument.
#' @param wgt.columns a named list that should specify columns in additional weight data; \sQuote{id},
#' \sQuote{datetime}, and \sQuote{weight} are required. \sQuote{datetime} is date
#' and time for weight measurement, which can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time separately
#' (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param check.path path to \sQuote{check} directory, where check files are
#' created. The default (NULL) will not produce any check files.
#' @param failflow_fn filename for duplicate flow data with rate zero
#' @param failnounit_fn filename for MAR data with missing unit
#' @param failunit_fn filename for MAR data with invalid unit
#' @param failnowgt_fn filename for infusion data with missing weight where unit
#' indicates weight is required
#' @param censor_date_fn filename containing censor times created with invalid dose data
#' @param infusion.unit acceptable unit for infusion data
#' @param bolus.unit acceptable unit for bolus data
#' @param bol.rate.thresh upper limit for bolus rate; values above this are invalid
#' @param rateunit acceptable unit for hourly rate; defaults to \sQuote{mcg/hr}
#' @param ratewgtunit acceptable unit for hourly rate by weight; defaults to \sQuote{mcg/kg/hr}
#' @param weightunit acceptable unit for weight; defaults to \sQuote{kg}
#' @param drugname drug of interest, included in filename of check files. The default (NULL)
#' will produce filenames without drugname included.
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return structured data set
#'
#' @examples 
#' # flow data for 'Fakedrug1'
#' flow <- data.frame(mod_id=c(1,1,2,2,2),
#'                    mod_id_visit=c(46723,46723,84935,84935,84935),
#'                    record.date=c("07/05/2019 5:25","07/05/2019 6:01",
#'                                  "09/04/2020 3:21", "09/04/2020 4:39",
#'                                  "09/04/2020 5:32"),
#'                    Final.Weight=c(6.75,6.75,4.5,4.5,4.5),
#'                    Final.Rate=c(rep("1 mcg/kg/hr",2),
#'                                 rep("0.5 mcg/kg/hr",3)),
#'                    Final.Units=c("3.375","6.5",
#'                                  "2.25","2.25","2.25"))
#' flow[,'Perform.Date'] <- pkdata::parse_dates(flow[,'record.date'])
#' flow[,'unit'] <- sub('.*[ ]', '', flow[,'Final.Rate'])
#' flow[,'rate'] <- as.numeric(sub('([0-9.]+).*', '\\1', flow[,'Final.Rate']))
#'
#' # mar data for 4 fake drugs
#' mar <- data.frame(mod_id=rep(1,5),
#'                   Date=rep("2019-07-05",5),
#'                   Time=c("07:12","07:31","08:47","09:16","10:22"),
#'                   `med:mDrug`=c("Fakedrug2","Fakedrug1","Fakedrug2",
#'                                 "Fakedrug3","Fakedrug4"),
#'                   `med:dosage`=c("30 mg","0.5 mcg","1 mg",
#'                                  "20 mg","3 mcg/kg/min"),
#'                   `med:route`=rep("IV",5),
#'                   `med:given`=rep("Given",5),
#'                   check.names=FALSE)
#' 
#' # medcheck file for drug of interest ('Fakedrug1')
#' medcheck <- data.frame(medname="Fakedrug1",freq=4672)
#' 
#' run_MedStrI(mar.path = mar,
#'             mar.columns = list(id = 'mod_id', datetime = c('Date','Time'),
#'                                dose = 'med:dosage', drug = 'med:mDrug', given = 'med:given'),
#'             flow.path = flow,
#'             flow.columns = list(id = 'mod_id', datetime = 'Perform.Date',
#'                                 finalunits = 'Final.Units', unit = 'unit',
#'                                 rate = 'rate', weight = 'Final.Weight'),
#'             medchk.path = medcheck,
#'             check.path = tempdir(),
#'             drugname = 'fakedrg1')
#'
#' @export

run_MedStrI <- function(mar.path,
                        mar.columns = list(),
                        medGivenReq = FALSE,
                        flow.path = NULL,
                        flow.columns = list(),
                        medchk.path = NULL,
                        demo.list = NULL,
                        demo.columns = list(),
                        missing.wgt.path = NULL,
                        wgt.columns = list(),
                        check.path = NULL,
                        failflow_fn = 'FailFlow',
                        failnounit_fn = 'NoUnit',
                        failunit_fn = 'Unit',
                        failnowgt_fn = 'NoWgt',
                        censor_date_fn = 'CensorTime',
                        infusion.unit = 'mcg/kg/hr',
                        bolus.unit = 'mcg',
                        bol.rate.thresh = Inf,
                        rateunit = 'mcg/hr',
                        ratewgtunit = 'mcg/kg/hr',
                        weightunit = 'kg',
                        drugname = NULL) {
  #### flow data

  # read and transform data
  if(!is.null(flow.path)) {
    flow1 <- read(flow.path)
    flow.req <- list(id = NA, datetime = NA, finalunits = NA, unit = NA, rate = NA, weight = NA, idvisit = NULL)
    flow.col <- validateColumns(flow1, flow.columns, flow.req)
    if(length(flow.col$datetime) == 2) {
      flowDT <- paste(flow1[,flow.col$datetime[1]], flow1[,flow.col$datetime[2]])
    } else {
      flowDT <- flow1[,flow.col$datetime]
    }
    flow1[,'date.time'] <- pkdata::parse_dates(flowDT)
    if('idvisit' %in% names(flow.col)) {
      flow.idv <- flow1[,flow.col$idvisit]
    } else {
      flow.idv <- flow1[,flow.col$id]
    }
    flow1 <- flow1[,c(flow.col$id, 'date.time', flow.col$finalunits, flow.col$unit, flow.col$rate, flow.col$weight)]
    names(flow1) <- c('mod_id','date.time','final.units','unit','rate','weight')
    flow1[,'mod_id_visit'] <- flow.idv
    # clean flow data (remove duplicates, missing rate/unit, incorrect units)
    medFlow <- flowData_mod(flow1, checkDir = check.path, failflow_filename = failflow_fn)
  } else {
    # no flow data, create mock with proper columns
    medFlow <- data.frame(mod_id = NA, date.time = NA, final.units = NA, unit = NA, rate = NA, weight = NA)[FALSE,]
  }

  #### MAR data
  mar.in <- read(mar.path)
  mar.req <- list(id = NA, datetime = NA, dose = NA, drug = NULL, weight = NULL, given = NULL)
  mar.col <- validateColumns(mar.in, mar.columns, mar.req)
  mar.idCol <- mar.col$id
  mar.datetimeCol <- mar.col$datetime
  mar.doseCol <- mar.col$dose
  mar.drugCol <- mar.col$drug
  mar.weightCol <- mar.col$weight
  mar.givenCol <- mar.col$given

  censor_opts <- vector('list', 3)

  if(!is.null(mar.drugCol) && !is.null(medchk.path)) {
    ## medChecked data
    list.med <- read(medchk.path)[['medname']]
    medMAR <- mar.in[!is.na(mar.in[,mar.drugCol]) & mar.in[,mar.drugCol] %in% list.med,]
  } else {
    medMAR <- mar.in
  }
  rm(mar.in)

  if(is.null(mar.givenCol)) {
    dm <- medMAR
  } else if(!medGivenReq) {
    # when set to NA, NA is a valid option
    dm <- medMAR[is.na(medMAR[,mar.givenCol]) | medMAR[,mar.givenCol] == 'Given',]
  } else {
    dm <- medMAR[!is.na(medMAR[,mar.givenCol]) & medMAR[,mar.givenCol] == 'Given',]
  }
  rm(medMAR)

  dm <- setDoseMar(dm, mar.doseCol, mar.datetimeCol, mar.weightCol)
  reqInfusionColumns <- c(mar.idCol, 'date.time', 'unit', 'rate', mar.weightCol)

  hasUnit <- !is.na(dm[,'unit'])
  # handle missing unit
  if(!is.null(check.path)) {
    dmnu <- dm[!hasUnit,]
    if(nrow(dmnu) > 0) {
      nounitfn <- file.path(check.path, paste0('fail', failnounit_fn,'-', drugname, '.csv'))
      nounitfixfn <- sub('fail', 'fix', nounitfn)

      mf <- cbind(dmnu[,reqInfusionColumns], dmnu[,mar.drugCol], flag = 'exclude')
      names(mf)[ncol(mf)-1] <- mar.drugCol
      msg <- sprintf('%s rows with no unit, see file %s AND create %s', nrow(mf), nounitfn, nounitfixfn)
      writeCheckData(mf, nounitfn, msg)

      if(file.access(nounitfixfn, 4) != -1) {
        hasfix <- read.csv(nounitfixfn, stringsAsFactors = FALSE)
        if(nrow(dmnu) != nrow(hasfix)) {
          wmsg <- sprintf('all rows in fix file %s should match respective rows in fail file %s\n', nounitfn, nounitfixfn)
          stop(wmsg)
        }
        toKeep <- hasfix[,'flag'] != 'exclude'
        nFixed <- sum(toKeep)
        nBad <- sum(!toKeep)
        if(nFixed > 0L) {
          # update required columns (ideally just "unit")
          getfix <- dtMirror(hasfix[toKeep, reqInfusionColumns], dmnu[toKeep, reqInfusionColumns])
          dmnu[toKeep, reqInfusionColumns] <- getfix[, reqInfusionColumns]
          message(sprintf('file %s read, %s records added', nounitfixfn, nFixed))
          dm0 <- setDoseMar(dmnu[toKeep,], mar.doseCol, 'date.time', mar.weightCol)
          dm1 <- rbind(dm[hasUnit,], dm0)
          # re-order data
          dm <- dm1[order(dm1[,mar.idCol], dm1[,'date.time']),]
          hasUnit <- !is.na(dm[,'unit'])
        }
        ## unfixed/bad data should be censored
        if(nBad > 0L) {
          censor_opts[[1]] <- dmnu[!toKeep, c(mar.idCol, 'date.time')]
        }
      } else {
        censor_opts[[1]] <- dmnu[, c(mar.idCol, 'date.time')]
      }
    }
  }

  unit <- dm[,'unit']
  inf0 <- dm[hasUnit & unit == infusion.unit,]
  inf1 <- inf0[,reqInfusionColumns]

  bol <- dm[hasUnit & unit == bolus.unit,]
  bol <- bol[bol[,'rate'] < bol.rate.thresh,] # for fent should be Inf, for dex should be 200

  dm <- dm[hasUnit & !(unit %in% c(bolus.unit, infusion.unit)),]
  unitfn <- file.path(check.path, paste0('fail', failunit_fn,'-', drugname, '.csv'))
  unitfixfn <- sub('fail', 'fix', unitfn)

  # demo data
  hasDemo <- !is.null(demo.list)
  if(hasDemo) { # if using demographic data
    demoData <- NULL
    if(inherits(demo.list, 'data.frame')) {
      demoData <- demo.list
    } else {
      if('demo' %in% names(demo.list)) {
        demoData <- demo.list$demo
      }
    }
    if(is.null(demoData)) {
      warning('Demographic data was provided in an unexpected format and will be ignored')
      hasDemo <- FALSE
    }
  }
  if(hasDemo) {
    demo.req <- list(id = NA, datetime = NA, weight = NA)
    demo.col <- validateColumns(demoData, demo.columns, demo.req)
    if(length(demo.col$datetime) == 2) {
      demoDT <- paste(demoData[,demo.col$datetime[1]], demoData[,demo.col$datetime[2]])
    } else {
      demoDT <- demoData[,demo.col$datetime]
    }
    demoData <- demoData[,c(demo.col$id, demo.col$weight)]
    names(demoData) <- c('mod_id','weight')
    demoData[,'date.time'] <- pkdata::parse_dates(demoDT)
  }

  hasMsWgt <- !is.null(missing.wgt.path)
  if(hasMsWgt) {
    # expected column order: ID|DATE|WGT
    missWgt <- read(missing.wgt.path)
    wgt.req <- list(id = NA, datetime = NA, weight = NA)
    wgt.col <- validateColumns(missWgt, wgt.columns, wgt.req)
    missWgt <- missWgt[,c(wgt.col$id, wgt.col$datetime, wgt.col$weight)]
    names(missWgt) <- c('mod_id','date.time','weight')
  } else {
    missWgt <- NULL
  }

  if(is.null(check.path)) {
    # inf1 will be used with records of any unknown units removed
  } else if(nrow(dm) > 0) {
    # impute missing weight
    lastid <- -1
    if(hasDemo) {
      demo.ids <- demoData[,'mod_id']
    }
    if(!is.null(mar.weightCol)) {
      nowgt_ix <- which(is.na(dm[,'weight']))
    } else {
      dm[,'weight'] <- NA
      nowgt_ix <- seq(nrow(dm))
    }
    dm[,'weight.date.time'] <- as.POSIXct(NA)
    # first, try flow
    # next, try missing wgt file
    # last, try demo
    for(i in seq(nrow(dm))) {
      row <- dm[i, c(mar.idCol,'date.time')]
      if(row[[1]] != lastid) {
        # try flow data
        opt <- medFlow[medFlow[,'mod_id'] == row[[1]], c('date.time', 'weight')]
        lastid <- row[[1]]
        # try missing weight data
        if(nrow(opt) == 0 && hasMsWgt) {
          opt <- missWgt[missWgt[,'mod_id'] == row[[1]], c('date.time','weight')]
        }
        # try demo data (if provided)
        if(nrow(opt) == 0 && hasDemo) {
          opt <- demoData[demo.ids == row[[1]], c('date.time', 'weight')]
        }
      }
      if(nrow(opt) > 0) {
        dm[i,'weight'] <- takeClosest(row[[2]], opt[[1]], opt[[2]])
        dm[i,'weight.date.time'] <- takeClosestTime(row[[2]], opt[[1]], opt[[2]])
      }
    }

    coi <- c(mar.idCol, 'date.time', mar.drugCol, 'rate', 'unit', 'weight', 'weight.date.time')
    mf <- cbind(dm[,coi], flag = 'exclude')
    msg <- sprintf('%s rows with units other than %s or %s, see file %s AND create %s',
                nrow(mf), infusion.unit, bolus.unit, unitfn, unitfixfn)
    writeCheckData(mf, unitfn, msg)

    if(file.access(unitfixfn, 4) != -1) {
      hasfix <- read.csv(unitfixfn, stringsAsFactors = FALSE)
      toKeep <- hasfix[,'flag'] != 'exclude'
      nFixed <- sum(toKeep)
      nBad <- sum(!toKeep)
      if(nFixed > 0L) {
        # recreate dates
        getfix <- dtMirror(hasfix[toKeep, reqInfusionColumns], inf0[,reqInfusionColumns])
        inf1 <- rbind(inf0[,reqInfusionColumns], getfix)
        message(sprintf('file %s read, %s records added', unitfixfn, nFixed))
      }
      ## unfixed/bad data should be censored
      if(nBad > 0L) {
        censor_opts[[2]] <- hasfix[!toKeep, c(mar.idCol, 'date.time')]
      }
    } else {
      censor_opts[[2]] <- mf[, c(mar.idCol, 'date.time')]
    }
  } else {
    message(sprintf('no units other than %s or %s, file %s not created', infusion.unit, bolus.unit, unitfn))
  }

  # infusionData_mod() accepts MAR data (inf1) with units
  # 'mcg/kg/hr' and weight --> multiply by weight to get dose/hr
  # 'mcg/hr' and no weight --> already formatted as dose/hr

  # combine missWgt with demoWgt
  w1 <- w2 <- NULL
  if(hasMsWgt) {
    w1 <- missWgt[,c('mod_id','date.time','weight')]
  }
  if(hasDemo) {
    w2 <- demoData[,c('mod_id','date.time','weight')]
  }
  moreWgt <- rbind(w1, w2)

  # remove id-visit column from flow if present
  mf <- medFlow[,setdiff(names(medFlow), 'mod_id_visit')]
  # combine flow and MAR infusion
  # `inf1` may or may not have "weight" column
  inf <- infusionData_mod(
    mf, inf1, rateunit = rateunit, ratewgtunit = ratewgtunit, addWgt = moreWgt
  )
  # infusionData_mod enforces mod_id; restore name
  if(mar.idCol != 'mod_id') {
    names(inf)[1] <- mar.idCol
  }

  # WARNING: if 'mcg/kg/hr' is missing weight, rate will be NA
  # e.g. observation has rate in mcg/kg/hr in MAR data (inf0), but no weight in flow data (medFlow)
  # so ultimate calculated rate is NA
  rnums <- which(grepl(weightunit, inf[,'unit']) & is.na(inf[,'weight']))
  if(!is.null(check.path) && length(rnums)) {
    needfix <- inf[rnums,]
    nofix <- inf[-rnums,]
    if('orig_rate' %in% names(inf)) {
      # preserve original rate
      needfix[,'rate'] <- needfix[,'orig_rate']
      needfix[,'orig_rate'] <- NULL
      nofix[,'orig_rate'] <- NULL
    }
    nowgtfn <- file.path(check.path, paste0('fail', failnowgt_fn,'-', drugname, '.csv'))
    nowgtfixfn <- sub('fail', 'fix', nowgtfn)
    msg <- sprintf('%s rows from %s subjects with "%s" in infusion unit but missing weight, see file %s AND create %s',
                nrow(needfix), length(unique(needfix[,mar.idCol])), weightunit, nowgtfn, nowgtfixfn)
    writeCheckData(needfix, nowgtfn, msg)

    if(file.access(nowgtfixfn, 4) != -1) {
      hasfix <- read.csv(nowgtfixfn, stringsAsFactors = FALSE)
      isFixed <- which(!is.na(hasfix[,'rate']) & !is.na(hasfix[,'weight']))
      if(length(isFixed)) {
        # multiply hourly rate per weight unit by weight to get hourly rate
        # note: this is awkward, would be better to provide data in `missing.wgt.path`
        # rather than a fix file
        hasfix[,'rate'] <- hasfix[,'rate'] * hasfix[,'weight']
        # recreate dates
        getfix <- dtMirror(hasfix, nofix)
        inf <- rbind(nofix, getfix)
        inf <- inf[order(inf[,mar.idCol], inf[,'date.time']),]
        message(sprintf('file %s read, %s records corrected', nowgtfixfn, nrow(hasfix)))
      }
    } else {
      # no fix file
      censor_opts[[3]] <- needfix[, c(mar.idCol, 'date.time')]
    }
  }
  # original rate may have been preserved for fix file - delete it now
  if('orig_rate' %in% names(inf)) {
    inf[,'orig_rate'] <- NULL
  }

  # combine infusion and bolus
  hourly <- merge_inf_bolus(inf, bol[,c(mar.idCol,'date.time','rate')])
  if(nrow(hourly) == 0) {
    stop('after processing, no infusion or bolus data exists')
  }
  # merge_inf_bolus enforces mod_id; restore name
  if(mar.idCol != 'mod_id') {
    names(hourly)[1] <- mar.idCol
  }

  # make sure times are POSIXct
  for(i in seq_along(censor_opts)) {
    if(!is.null(censor_opts[[i]])) {
      censor_opts[[i]][,'date.time'] <- pkdata::parse_dates(censor_opts[[i]][,'date.time'])
    }
  }
  censor_opts <- do.call(rbind, censor_opts)
  if(is.null(check.path)) {
    warning('no check.path is provided; censor dates will not be utilized')
  } else if(!is.null(censor_opts)) {
    # find "min" date-time for mod_id
    censor_opts <- censor_opts[order(censor_opts[,mar.idCol], censor_opts[,'date.time']),]
    censor_date <- censor_opts[!duplicated(censor_opts[,mar.idCol]),]
    cdfn <- file.path(check.path, paste0(censor_date_fn,'-', drugname, '.csv'))
    msg <- sprintf('censor dates created, please see %s', cdfn)
    writeCheckData(censor_date, cdfn, msg)
  }

  cdArgs <- list(
    doseData = hourly, idVar = mar.idCol, dateVar = "date.dose",
    otherDoseTimeVar = NULL, otherDoseVar = NULL, otherVars = c('given.dose','maxint','weight')
  )
  # hourly data is already merged, but ensure data is present before updating arguments
  if(nrow(inf) > 0) {
    cdArgs$infusionDoseTimeVar <- 'infuse.time'
    cdArgs$infusionDoseVar <- 'infuse.dose'
  }
  if(nrow(bol) > 0) {
    cdArgs$bolusDoseTimeVar <- 'bolus.time'
    cdArgs$bolusDoseVar <- 'bolus.dose'
  }
  d1 <- do.call(pkdata::conformDoses, cdArgs)
  d1
}
