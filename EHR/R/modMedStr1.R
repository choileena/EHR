#' Run Str Data
#'
#' This module will load and modify infusion and bolus data.
#'
#' @param flow.path filename of flow data (stored as RDS)
#' @param flow.select columns to select
#' @param flow.rename new column names for flow data
#' @param flow.mod.list list of expressions, giving modifications to make
#' @param medchk.path filename containing data set (stored as CSV); should have
#' the column \sQuote{medname} with list of acceptable drug names used to filter
#' MAR data
#' @param mar.path filename of MAR data (stored as RDS)
#' @param demo.list demographic information; if available, missing weight may be
#' imputed from demographics
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
#' @param drugname drug of interest, included in filename of check files
#'
#' @return str data set
#'
#' @export

run_MedStrI <- function(flow.path, 
                        flow.select = c('mod_id','mod_id_visit','Perform.Date','Final.Wt..kg.','Final.Rate..NFR.units.','Final.Units'),
                        flow.rename = c('mod_id','mod_id_visit', 'Perform.Date', 'weight', 'rate', 'final.units'),
                        flow.mod.list = list(
                          date.time = expression(parse_dates(fixDates(Perform.Date))),
                          unit = expression(sub('.*[ ]', '', rate)),
                          rate = expression(as.numeric(sub('([0-9.]+).*', '\\1', rate)))),
                        medchk.path, 
                        mar.path,
                        demo.list = NULL,
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
                        drugname) {
  #### flow data

  # read and transform data
  if(!missing(flow.path)) {
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
  dm <- medMAR[!is.na(medMAR[,'med:given']) & medMAR[,'med:given'] == 'Given',]
  unit <- sub('.*[ ]', '', dm[,'med:dosage'])
  rate <- suppressWarnings(as.numeric(sub('([0-9.]+).*', '\\1', dm[,'med:dosage'])))
  dm[,'unit'] <- unit
  dm[,'rate'] <- rate
  dm[,'date.time'] <- parse_dates(paste(dm[,'Date'], dm[,'Time']))
  inf0 <- dm[unit == infusion.unit,]
  inf1 <- inf0[,c('mod_id','date.time','unit','rate')]

  bol <- dm[unit == bolus.unit,]
  bol <- bol[bol[,'rate'] < bol.rate.thresh,] # for fent should be Inf, for dex should be 200

  dm <- dm[!is.na(dm[,'unit']) & !(dm[,'unit'] %in% c(bolus.unit, infusion.unit)),]
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

  if(nrow(dm) > 0) {
    # impute missing weight 
    lastid <- -1
    if(hasDemo) {
      demo.ids <- demoData[,'mod_id']
    }
    for(i in seq(nrow(dm))) {
      row <- dm[i, c('mod_id','date.time')]
      # try flow data
      if(row[[1]] != lastid) {
        opt <- medFlow[medFlow[,'mod_id'] == row[[1]], c('date.time','weight')]
        lastid <- row[[1]]
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

  # combine flow and MAR infusion

  # infusionData_mod() accepts MAR data (inf1) with units
  # 'mcg/kg/hr' and weight --> multiply by weight to get dose/hr
  # 'mcg/hr' and no weight --> already formatted as dose/hr

  inf <- infusionData_mod(
    medFlow[,c('mod_id','date.time','final.units','unit','rate','weight')], inf1, rateunit = rateunit, ratewgtunit = ratewgtunit
  )

  #!! add interactive check for missing flow weight
  # e.g. mod_id 13 has rate in mcg/kg/hr in MAR data (inf0), but no weight in flow data (medFlow)
  # so ultimate calculated rate is NA
  # subset(inf, id==13)
  # subset(inf1, mod_id==13)

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

  #subset(hourly, id %in% unique(inf_nowgt[,'id']))

  d1 <- pkdata::conformDoses(doseData=hourly, idVar="mod_id", dateVar="date.dose",
                            infusionDoseTimeVar="infuse.time", infusionDoseVar="infuse.dose",
                            bolusDoseTimeVar="bolus.time", bolusDoseVar="bolus.dose",
                            otherDoseTimeVar=NULL, otherDoseVar=NULL,
                            otherVars=c('given.dose','maxint','weight'))

  names(d1)[1] <- c("mod_id") #rename id to mod_id

  # subset(d1, mod_id %in% unique(inf_nowgt[,'id']))

  return(d1)
}
