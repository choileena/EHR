#' Run Drug Level Data
#'
#' This module will load and modify drug-level data.
#'
#' @param conc.path filename of concentration data (CSV, RData, RDS), or data.frame
#' @param conc.columns a named list that should specify columns in concentration data.
#' \sQuote{id} and \sQuote{conc} are required. \sQuote{idvisit} may also be specified.
#' If linking with sampling data, \sQuote{samplinkid} is required. Otherwise
#' \sQuote{datetime} is required. This is the date and time when blood samples were
#' obtained. This can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time
#' separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param conc.select columns to select from concentration data
#' @param conc.rename new column names for concentration data
#' @param conc.mod.list list of expressions, giving modifications to make
#' @param samp.path filename of data with sampling time (CSV, RData, RDS), or data.frame
#' @param samp.columns a named list that should specify columns in sampling data.
#' \sQuote{conclinkid} and \sQuote{datetime} are required to link sampling data to
#' concentration data. \sQuote{conclinkid} should match the id variable provided as
#' \sQuote{samplinkid} in the \sQuote{conc.columns} argument. \sQuote{datetime} is the
#' date and time when blood samples were obtained. This can refer to a single date-time
#' variable (datetime = \sQuote{date_time}) or two variables holding date and time
#' separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param samp.mod.list list of expressions, giving modifications to make
#' @param check.path path to \sQuote{check} directory, where check files are
#' created. The default (NULL) will not produce any check files.
#' @param failmiss_fn filename for data missing concentration date
#' @param multsets_fn filename for data with multiple concentration sets
#' @param faildup_fn filename for data with duplicate concentration observations
#' @param drugname drug of interest, included in filename of check files. The default (NULL)
#' will produce filenames without drugname included.
#' @param LLOQ lower limit of concentration values; values below this are invalid
#' @param demo.list demographic information; if available, concentration records
#' must have a valid demo record
#' @param demo.columns a named list that should specify columns in demographic data; \sQuote{id},
#' is required. If \sQuote{idvisit} is present in the concentration data, then it is required
#' here too.
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return drug-level data set
#'
#' @examples
#' # concentrations
#' conc_data <- data.frame(mod_id = rep(1:3,each=4),
#'                        mod_visit = rep(c(2,1,1),each=4),
#'                        mod_id_visit = as.numeric(paste(rep(1:3,each=4),
#'                                                       rep(c(2,1,1),each=4), sep=".")),
#'                        samp = rep(1:4,times=3),
#'                        drug_calc_conc=15*exp(-1*rep(1:4,times=3))+rnorm(12,0,0.1))
#'
#' # sample times
#' build_date <- function(x) format(seq(x, length.out=4, by="1 hour"), "%Y-%m-%d %H:%M")
#' dates <- unlist(lapply(rep(Sys.time(),3), build_date))
#'
#' samp_data <- data.frame(mod_id = rep(1:3,each=4),
#'                        mod_visit = rep(c(2,1,1),each=4),
#'                        mod_id_visit = as.numeric(paste(rep(1:3,each=4),
#'                                                        rep(c(2,1,1),each=4), sep=".")),
#'                        samp = rep(1:4,times=3),
#'                        Sample.Collection.Date.and.Time = dates)
#' 
#' run_DrugLevel(
#'   conc.path = conc_data,
#'   conc.columns = list(
#'     id = 'mod_id', idvisit = 'mod_id_visit', samplinkid = 'mod_id_event', conc = 'conc.level'
#'   ),
#'   conc.select = c('mod_id','mod_id_visit','samp','drug_calc_conc'),
#'   conc.rename = c(drug_calc_conc= 'conc.level', samp='event'),
#'   conc.mod.list = list(mod_id_event = expression(paste(mod_id_visit, event, sep = "_"))),
#'   samp.path = samp_data,
#'   samp.columns = list(conclinkid = 'mod_id_event', datetime = 'Sample.Collection.Date.and.Time'),
#'   samp.mod.list = list(mod_id_event = expression(paste(mod_id_visit, samp, sep = "_"))),
#'   drugname = 'drugnm',
#'   LLOQ = 0.05
#' )
#'
#' # minimal example with data in required format
#' conc_data <- conc_data[,c('mod_id','mod_id_visit','samp','drug_calc_conc')]
#' conc_data[,'mod_id_event'] <- paste(conc_data[,'mod_id_visit'], conc_data[,'samp'], sep = "_")
#' names(conc_data)[3:4] <- c('event','conc.level')
#' samp_data[,'mod_id_event'] <- paste(samp_data[,'mod_id_visit'], samp_data[,'samp'], sep = "_")
#' conc_samp_link <- match(conc_data[,'mod_id_event'], samp_data[,'mod_id_event'])
#' conc_date <- samp_data[conc_samp_link, 'Sample.Collection.Date.and.Time']
#' conc_data[,'date.time'] <- as.POSIXct(conc_date)
#' run_DrugLevel(conc_data, conc.columns = list(
#'   id = 'mod_id', idvisit = 'mod_id_visit', datetime = 'date.time', conc = 'conc.level'
#' ))
#'
#' @export

run_DrugLevel <- function(conc.path, conc.columns = list(), conc.select, conc.rename,
                          conc.mod.list = NULL,
                          samp.path = NULL, samp.columns = list(),
                          samp.mod.list = NULL,
                          check.path = NULL,
                          failmiss_fn = 'MissingConcDate-',
                          multsets_fn = 'multipleSetsConc-',
                          faildup_fn = 'DuplicateConc-',
                          drugname = NULL, LLOQ = NA,
                          demo.list = NULL, demo.columns = list()) {

  #### sample
  if(!is.null(samp.path)) {
    # read and transform data
    samp.in <- read(samp.path)
    samp <- dataTransformation(samp.in, modify = samp.mod.list)
    samp.req <- list(conclinkid = NA, datetime = NA)
    samp.col <- validateColumns(samp, samp.columns, samp.req)
    if(length(samp.col$datetime) == 2) {
      sampDT <- paste(samp[,samp.col$datetime[1]], samp[,samp.col$datetime[2]])
    } else {
      sampDT <- samp[,samp.col$datetime]
    }
    samp[,'datetime'] <- sampDT
    # require samp_link_id, to merge date-time from sample
    conc.req <- list(id = NA, idvisit = NULL, samplinkid = NA, conc = NA)
  } else {
    samp <- NULL
    # require date-time
    conc.req <- list(id = NA, idvisit = NULL, datetime = NA, conc = NA)
    samp.col <- NULL
  }

  #### concentration
  # read and transform data
  conc.in <- read(conc.path)
  conc <- dataTransformation(conc.in,
    select = conc.select,
    rename = conc.rename,                         
    modify = conc.mod.list
  )
  conc.col <- validateColumns(conc, conc.columns, conc.req)
  if('datetime' %in% names(conc.col)) {
    if(length(conc.col$datetime) == 2) {
      concDT <- paste(conc[,conc.col$datetime[1]], conc[,conc.col$datetime[2]])
    } else {
      concDT <- conc[,conc.col$datetime]
    }
    conc[,'datetime'] <- concDT
  }

  #### demo data
  demoData <- NULL
  hasDemo <- !is.null(demo.list)
  if(hasDemo) { # if using demographic data
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
    # demo will be used to restrict conc to IDs of interest
    # use `id` variable unless `idvisit` is present
    if('idvisit' %in% names(conc.col)) {
      demo.req <- list(id = NA, idvisit = NA)
    } else {
      demo.req <- list(id = NA)
    }
    demo.col <- validateColumns(demoData, demo.columns, demo.req)
  } else {
    demo.col <- NULL
  }

  conc.out <- concData_mod(conc, samp, lowerLimit=LLOQ, drugname=drugname,
                          checkDir = check.path, 
                          dem = demoData,
                          failmissconc_filename = failmiss_fn,
                          multsets_filename = multsets_fn,
                          faildupconc_filename = faildup_fn,
                          conc.columns = conc.col, samp.columns = samp.col, demo.columns = demo.col)

  return(conc.out)
}
