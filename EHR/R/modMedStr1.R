#' Run Str Data I
#'
#' This module will load and modify structured intravenous (IV) infusion and 
#' bolus medication data.
#'
#' @param flow.path filename of flow data (stored as RDS)
#' @param flow.select existing column names to select for flow data
#' @param flow.rename new column names for flow data
#' @param flow.mod.list list of expressions, giving modifications to flow data
#' @param medchk.path filename containing data set (stored as CSV); should have
#' the column \sQuote{medname} with list of acceptable drug names used to filter
#' MAR data
#' @param mar.path filename of MAR data (stored as RDS)
#' @param demo.list demographic information; if available, missing weight may be
#' imputed from demographics
#' @param missing.wgt.path filename to additional weight data
#' @param check.path path to \sQuote{check} directory, where check files are
#' created
#' @param failflow_fn filename for duplicate flow data with rate zero
#' @param failunit_fn filename for MAR data with invalid unit
#' @param failnowgt_fn filename for infusion data with missing weight where unit
#' indicates weight is required
#' @param infusion.unit acceptable unit for infusion data
#' @param bolus.unit acceptable unit for bolus data
#' @param bol.rate.thresh upper limit for bolus rate; values above this are invalid
#' @param rateunit acceptable unit for hourly rate; defaults to \sQuote{mcg/hr}
#' @param ratewgtunit acceptable unit for hourly rate by weight; defaults to \sQuote{mcg/kg/hr}
#' @param weightunit acceptable unit for weight; defaults to \sQuote{kg}
#' @param medGivenReq values for \sQuote{med:given} should equal \dQuote{Given} unless this is FALSE
#' @param drugname drug of interest, included in filename of check files
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return str data set
#'
#' @examples 
#' \dontrun{
#' # flow data for 'Fakedrug1'
#' flow <- data.frame(mod_id=c(1,1,2,2,2),
#'                    mod_id_visit=c(46723,46723,84935,84935,84935),
#'                    record.date=c("7/5/2019 5:25","7/5/2019 6:01",
#'                                  "9/4/2020 3:21", "9/4/2020 4:39",
#'                                  "9/4/2020 5:32"),
#'                    Final.Weight=c(6.75,6.75,4.5,4.5,4.5),
#'                    Final.Rate=c(rep("1 mcg/kg/hr",2),
#'                                 rep("0.5 mcg/kg/hr",3)),
#'                    Final.Units=c("3.375","6.5",
#'                                  "2.25","2.25","2.25"))
#' 
#' saveRDS(flow, 'flow.rds')
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
#' saveRDS(mar, 'mar.rds')
#' 
#' # medcheck file for drug of interest ('Fakedrug1')
#' medcheck <- data.frame(medname="Fakedrug1",freq=4672)
#' 
#' write.csv(medcheck, 'medcheck.csv')
#' 
#' 
#' run_MedStrI(flow.path='flow.rds',
#'             flow.select=c("mod_id","mod_id_visit","record.date",
#'                           "Final.Weight", "Final.Rate","Final.Units"),
#'             flow.rename = c("mod_id", "mod_id_visit", "Perform.Date",
#'                             "weight", "rate","final.units"),
#'             medchk.path='medcheck.csv',
#'             mar.path='mar.rds',
#'             check.path=tempdir(),
#'             drugname='fakedrg1')
#'}
#'
#' @export

run_MedStrI <- function(flow.path = NULL,
                        flow.select = c('mod_id','mod_id_visit','Perform.Date','Final.Wt..kg.','Final.Rate..NFR.units.','Final.Units'),
                        flow.rename = c('mod_id','mod_id_visit', 'Perform.Date', 'weight', 'rate', 'final.units'),
                        flow.mod.list = list(
                          date.time = expression(parse_dates(fixDates(Perform.Date))),
                          unit = expression(sub('.*[ ]', '', rate)),
                          rate = expression(as.numeric(sub('([0-9.]+).*', '\\1', rate)))),
                        medchk.path, 
                        mar.path,
                        demo.list = NULL,
                        missing.wgt.path = NULL,
                        check.path,
                        failflow_fn = 'FailFlow',
                        failunit_fn = 'Unit',
                        failnowgt_fn = 'NoWgt',
                        infusion.unit = 'mcg/kg/hr',
                        bolus.unit = 'mcg',
                        bol.rate.thresh = Inf,
                        rateunit = 'mcg/hr',
                        ratewgtunit = 'mcg/kg/hr',
                        weightunit = 'kg',
                        medGivenReq = TRUE,
                        drugname) {
  #### flow data

  # read and transform data
  if(!is.null(flow.path)) {
    flow.in <- readRDS(flow.path)
    flow1 <- dataTransformation(flow.in,
                                select = flow.select,
                                rename = flow.rename,
                                modify = flow.mod.list)

    # clean flow data (remove duplicates, missing rate/unit, incorrect units)
    medFlow <- flowData_mod(flow1, checkDir = check.path, failflow_filename = failflow_fn)
  } else {
    # no flow data, create mock with proper columns
    medFlow <- data.frame(mod_id = NA, date.time = NA, final.units = NA, unit = NA, rate = NA, weight = NA)[FALSE,]
  }

  #### MAR data
  mar.in <- readRDS(mar.path)

  ## medChecked data
  list.med <- readTransform(file = medchk.path, select = 'medname')

  medMAR <- mar.in[!is.na(mar.in[,'med:mDrug']) & mar.in[,'med:mDrug'] %in% list.med,]
  if(medGivenReq) {
    dm <- medMAR[!is.na(medMAR[,'med:given']) & medMAR[,'med:given'] == 'Given',]
  } else {
    dm <- medMAR[is.na(medMAR[,'med:given']) | medMAR[,'med:given'] == 'Given',]
  }
  unit <- sub('.*[ ]', '', dm[,'med:dosage'])
  rate <- suppressWarnings(as.numeric(sub('([0-9.]+).*', '\\1', dm[,'med:dosage'])))
  dm[,'unit'] <- unit
  dm[,'rate'] <- rate
  dm[,'date.time'] <- parse_dates(paste(dm[,'Date'], dm[,'Time']))
  hasUnit <- !is.na(unit)
  inf0 <- dm[hasUnit & unit == infusion.unit,]
  inf1 <- inf0[,c('mod_id','date.time','unit','rate')]

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

  hasMsWgt <- !is.null(missing.wgt.path)
  if(hasMsWgt) {
    # expected column order: ID|DATE|WGT
    missWgt <- readRDS(missing.wgt.path)
    names(missWgt)[1:3] <- c('mod_id','date.time','weight')
  } else {
    missWgt <- NULL
  }

  if(nrow(dm) > 0) {
    # impute missing weight
    lastid <- -1
    if(hasDemo) {
      demo.ids <- demoData[,'mod_id']
    }
    # first, try flow
    # next, try missing wgt file
    # last, try demo
    for(i in seq(nrow(dm))) {
      row <- dm[i, c('mod_id','date.time')]
      # try flow data
      if(row[[1]] != lastid) {
        opt <- medFlow[medFlow[,'mod_id'] == row[[1]], c('date.time','weight')]
        lastid <- row[[1]]
      }
      if(nrow(opt) == 0 && hasMsWgt) {
        opt <- missWgt[missWgt[,'mod_id'] == row[[1]], c('date.time','weight')]
      }
      # try demo data (if provided)
      if(nrow(opt) == 0 && hasDemo) {
        dlix <- demoData[demo.ids == row[[1]], c('dateplusdospccu', 'weight')]
        opt <- data.frame(date.time = as.POSIXct(dlix[,1], format="%m/%d/%Y %H:%M"), weight = dlix[,2])
      }
      if(nrow(opt) > 0) {
        dm[i,'weight'] <- takeClosest(row[[2]], opt[[1]], opt[[2]])
        dm[i,'weight.date.time'] <- takeClosestTime(row[[2]], opt[[1]], opt[[2]])
      } else {
        dm[i,'weight'] <- NA
        dm[i,'weight.date.time'] <- NA
      }
    }

    coi <- c('mod_id','date.time','med:mDrug','rate','unit','weight','weight.date.time')
    mf <- cbind(dm[,coi], flag = 'exclude')
    msg <- sprintf('%s rows with units other than %s or %s, see file %s AND create %s\n',
                nrow(mf),infusion.unit, bolus.unit, unitfn, unitfixfn)
    writeCheckData(mf, unitfn, msg)

    if(file.access(unitfixfn, 4) != -1) {
      hasfix <- read.csv(unitfixfn, stringsAsFactors = FALSE)
      hasfix <- hasfix[hasfix[,'flag'] == 'keep',c('mod_id','date.time','unit','rate')]
        if(nrow(hasfix)) {
          inf1 <- rbind(inf0[,c('mod_id','date.time','unit','rate')], hasfix)
          cat(sprintf('file %s read, %s records added\n', unitfixfn, nrow(hasfix)))
        }
      }
  } else {
    cat(sprintf('no units other than %s or %s, file %s not created\n',
                infusion.unit, bolus.unit, unitfn))
  }

  # infusionData_mod() accepts MAR data (inf1) with units
  # 'mcg/kg/hr' and weight --> multiply by weight to get dose/hr
  # 'mcg/hr' and no weight --> already formatted as dose/hr

  # combine flow and MAR infusion
  inf <- infusionData_mod(
    medFlow[,c('mod_id','date.time','final.units','unit','rate','weight')], inf1, rateunit = rateunit, ratewgtunit = ratewgtunit, addWgt = missWgt
  )

  # WARNING: if 'mcg/kg/hr' is missing weight, rate will be NA
  # e.g. observation has rate in mcg/kg/hr in MAR data (inf0), but no weight in flow data (medFlow)
  # so ultimate calculated rate is NA
  rnums <- which(grepl(weightunit, inf[,'unit']) & is.na(inf[,'weight']))
  if(length(rnums)) {
    needfix <- inf[rnums,]
    nofix <- inf[-rnums,]
    nowgtfn <- file.path(check.path, paste0('fail', failnowgt_fn,'-', drugname, '.csv'))
    nowgtfixfn <- sub('fail', 'fix', nowgtfn)
    msg <- sprintf('%s rows from %s subjects with "%s" in infusion unit but missing weight, see file %s AND create %s\n',
                nrow(needfix), length(unique(needfix[,'mod_id'])), weightunit, nowgtfn, nowgtfixfn)
    writeCheckData(needfix, nowgtfn, msg)

    if(file.access(nowgtfixfn, 4) != -1) {
      hasfix <- read.csv(nowgtfixfn, stringsAsFactors = FALSE)
      if(nrow(hasfix)) {
        inf <- rbind(nofix, hasfix[,names(nofix)])
        inf <- inf[order(inf$mod_id, inf$date.time),]
        cat(sprintf('file %s read, %s records corrected\n', nowgtfixfn, nrow(hasfix)))
      }
    }
  }

  # combine infusion and bolus
  hourly <- merge_inf_bolus(inf, bol[,c('mod_id','Date','Time','rate','med:route')])
  if(nrow(hourly) == 0) {
    stop('after processing, no infusion or bolus data exists')
  }

  cdArgs <- list(
    doseData=hourly, idVar="mod_id", dateVar="date.dose",
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
