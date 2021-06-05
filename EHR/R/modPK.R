#' Build-PK-IV Module
#'
#' This module builds PK data for intravenously (IV) administered medications.
#'
#' @param conc concentration data, the output of \code{\link{run_DrugLevel}} 
#' or a correctly formatted data.frame
#' @param dose dose data, the output of \code{\link{run_MedStrI}} or a
#' correctly formatted data.frame
#' @param lab.dat lab data, if available. the output from \code{\link{run_Labs}} or 
#' a correctly formatted list
#' @param lab.vars variables to include from lab data
#' @param demo.list demographic information, if available. the output from 
#' \code{\link{run_Demo}} or a correctly formatted data.frame
#' @param demo.vars variables to include from demographic data
#' @param demo.abbr character vector used to rename/abbreviate demo variables
#' @param pk.vars variables to include from PK data. the variables 
#' c('mod_id_visit', 'time', 'conc', 'dose', 'rate', 'event') are required
#' @param drugname drug of interest, included in filename of check files
#' @param check.path path to \sQuote{check} directory, where check files are
#' created
#' @param missdemo_fn filename for checking NA frequency among demographic data
#' @param faildupbol_fn filename for duplicate bolus data
#' @param date.format output format for \sQuote{date} variable
#' @param date.tz output time zone for \sQuote{date} variable
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return PK data set
#'
#' @examples 
#' \dontrun{
#' 
#' # make fake data
#' set.seed(6543)
#' 
#' build_date <- function(x) as.character(seq(x, length.out=5, by="1 hour"))
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
#' run_Build_PK_IV(conc = plconc, dose = ivdose,
#'                 pk.vars = c('mod_id_visit', 'time', 'conc', 'dose', 'rate', 'event'))
#'}
#'
#' @export

run_Build_PK_IV <- function(conc, dose, lab.dat = NULL, lab.vars = NULL,
                            demo.list = NULL, demo.vars = NULL, demo.abbr = NULL, 
                            pk.vars, drugname, check.path, 
                            missdemo_fn='-missing-demo',
                            faildupbol_fn='DuplicateBolus-',
                            date.format="%m/%d/%y %H:%M:%S",
                            date.tz="America/Chicago") {
  # trim Doses - determine whether each dose is valid by comparing to concentration data
  tdArgs <- list(doseData=dose, drugLevelData=conc, drugLevelID="mod_id",
    drugLevelTimeVar="date.time", drugLevelVar="conc.level",
    otherDoseTimeVar=NULL, otherDoseVar=NULL
  )
  hasInf <- 'infuse.dose' %in% names(dose)
  hasBol <- 'bolus.dose' %in% names(dose)
  if(hasInf) {
    tdArgs$infusionDoseTimeVar <- 'infuse.time'
    tdArgs$infusionDoseVar <- 'infuse.dose'
  }
  if(hasBol) {
    tdArgs$bolusDoseTimeVar <- 'bolus.time'
    tdArgs$bolusDoseVar <- 'bolus.dose'
  }
  info <- do.call(pkdata::trimDoses, tdArgs)

  info <- resolveDoseDups_mod(info, checkDir=check.path, drugname=drugname, faildupbol_filename=faildupbol_fn)

  if('maxint' %in% names(info)) {
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

  hasDemo <- !is.null(demo.list)
  hasLabs <- !is.null(lab.dat)
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
      info1 <- info0
      hasDemo <- FALSE
    } else {
      dem <- demoData[,c('mod_id','surgery_date','time_fromor')]
      dem[dem == ''] <- NA
      info1 <- updateInterval_mod(info0, dem)
    }
  } else {
    info1 <- info0
  }

  doseById <- split(info1, info1[,'mod_id'])
  drugLevelById <- split(conc, conc[,'mod_id'])
  uids <- as.character(unique(conc[,'mod_id']))
  # ID needs to be in both data sets
  uids <- uids[uids %in% names(doseById)]
  # default pkdata arguments
  pkArgs <- list(doseIdVar = "mod_id", drugLevelVar="conc.level", intervalVar='maxint')
  if(hasInf) {
    pkArgs$infusionDoseTimeVar <- 'infuse.time'
    pkArgs$infusionDoseVar <- 'infuse.dose'
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
    cat(sprintf('The dimension of the PK data before merging with demographics: %s x %s\n', nrow(pkd), ncol(pkd)))
    cat(sprintf('The number of subjects in the PK data before merging with demographics: %s\n', length(unique(pkd$mod_id))))
  }

  hasMIV <- 'mod_id_visit' %in% names(conc)
  if(hasMIV) {
    pkd[,'mod_id_visit'] <- conc[match(pkd[,'mod_id'], conc[,'mod_id']), 'mod_id_visit']
  } else {
    pkd[,'mod_id_visit'] <- pkd[,'mod_id']
  }

  if(hasInf) {
    flow.weight <- info[!is.na(info[,'weight']), c('mod_id','infuse.time.real','weight')]
    tmp <- merge(pkd, flow.weight, by.x=c('mod_id','date'), by.y=c('mod_id','infuse.time.real'), all.x=TRUE)
  } else {
    tmp <- pkd
    tmp[,'weight'] <- NA_real_
  }

  if(!hasMIV) {
    tmp[,'mod_id_visit'] <- tmp[,'mod_id']
  }

  if(hasLabs) {
    for(i in seq_along(lab.dat)) {
      tmp <- merge_by_time(tmp, lab.dat[[i]], maxTime=168, x.id='mod_id', y.id='mod_id', x.time='date', y.time='date.time')
    }
    missLab <- setdiff(lab.vars, names(tmp))
    if(length(missLab)) {
      stop(sprintf('there was a problem merging lab variables: %s', paste(missLab, collapse = ', ')))
    }
  }

  datetime <- as.POSIXct(tmp[,'date'])
  tmp[,'date'] <- as.character(datetime, format = date.format, tz = date.tz)

  if(hasDemo) {
    tmp <- merge(tmp, demoData, by.x=c('mod_id_visit', 'mod_id'), by.y=c('mod_id_visit', 'mod_id'), all.x=TRUE)
    ix <- which(is.na(tmp[,'weight.x']))
    tmp[ix,'weight.x'] <- tmp[ix,'weight.y']
    names(tmp)[match(c('weight.x','weight.y'), names(tmp))] <- c('weight','weight_demo')

    # drop mod_id based on exclusion criteria
    cat(sprintf('The number of subjects in the demographic file, who meet the exclusion criteria: %s\n', length(demoExcl)))
    tmp <- tmp[!(tmp[,'mod_id_visit'] %in% demoExcl),]

    #drop if mod_id is missing (i.e. no demographics for this visit)
    tmp <- tmp[!(is.na(tmp[,'mod_id'])),]

    # check for missing demo
    dd2 <- tmp[tmp$event==0,]

    x <- data.frame(variable = colnames(dd2), freq = colSums(is.na(dd2)))
    x[,'percent'] <- round(x[,'freq'] / nrow(dd2), 2)
    rownames(x) <- NULL
    fn <- file.path(check.path, paste0(drugname, missdemo_fn, '.csv'))
    msg <- sprintf('check NA frequency in demographics, see file %s\n', fn)
    cat(msg)
    write.csv(x, fn, quote=FALSE, row.names=FALSE)

    missCpb <- tmp[is.na(tmp[,'cpb_sts']), 'mod_id_visit']
    cat(sprintf('List of IDs missing at least 1 cpb_sts: %s\n', paste(unique(missCpb), collapse = '\n')))
    if(length(missCpb) == 0) {
      cat('Checked: all missing cpb_sts are 0\n')
    } else {
      tmp[is.na(tmp[,'cpb_sts']), 'cpb_sts'] <- 0
    }

    cat(sprintf('The list of final demographic variables: %s\n', paste(demo.vars, collapse = '\n')))
  }

  if(hasLabs) {
    for(i in seq_along(lab.vars)) {
      varLabel <- lab.vars[i]
      missVar <- tmp[is.na(tmp[,varLabel]), 'mod_id_visit']
      if(length(missVar) == 0) {
        msg <- sprintf('Checked: there are no missing %s\n', varLabel)
      } else {
        msg <- sprintf('List of IDs missing at least 1 %s: %s\n', varLabel, paste(unique(missVar), collapse = '\n'))
      }
      cat(msg)
    }
  }

  keep.variable <- c(pk.vars, demo.vars, lab.vars)
  tmp2 <- tmp[, keep.variable]
  tmp2$mdv <- tmp2$event

  tmp3 <- tmp2[,c('mod_id_visit', 'time', 'conc', 'dose', 'rate', 'mdv', 'event', demo.vars, lab.vars)]

  if(hasDemo) {
    msg <- 'The dimension of the final PK data exported with the key demographics: %s x %s with %s distinct subjects (mod_id_visit)\n'
  } else {
    msg <- 'The dimension of the final PK data: %s x %s with %s distinct subjects (mod_id_visit)\n'
  }
  cat(sprintf(msg, nrow(tmp3), ncol(tmp3), length(unique(tmp3$mod_id_visit))))

  colnames(tmp3) <- c('mod_id_visit', 'time', 'conc', 'amt', 'rate', 'mdv', 'evid', demo.abbr, lab.vars)
  tmp3
}
