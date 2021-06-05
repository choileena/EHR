#' Run Drug Level Data
#'
#' This module will load and modify drug-level data.
#'
#' @param conc.path filename of concentration data (stored as RDS)
#' @param conc.select columns to select from concentration data
#' @param conc.rename new column names for concentration data
#' @param conc.mod.list list of expressions, giving modifications to make
#' @param samp.path filename of data with sampling time (stored as RDS)
#' @param samp.mod.list list of expressions, giving modifications to make
#' @param check.path path to \sQuote{check} directory, where check files are
#' created
#' @param failmiss_fn filename for data missing concentration date
#' @param multsets_fn filename for data with multiple concentration sets
#' @param faildup_fn filename for data with duplicate concentration observations
#' @param drugname drug of interest, included in filename of check files
#' @param LLOQ lower limit of concentration values; values below this are invalid
#' @param demo.list demographic information; if available, concentration records
#' must have a valid demo record
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return drug-level data set
#'
#' @examples
#' \dontrun{
#' # concentrations
#' conc_data <- data.frame(mod_id = rep(1:3,each=4),
#'                        mod_visit = rep(c(2,1,1),each=4),
#'                        mod_id_visit = as.numeric(paste(rep(1:3,each=4),
#'                                                       rep(c(2,1,1),each=4), sep=".")),
#'                        samp = rep(1:4,times=3),
#'                        drug_calc_conc=15*exp(-1*rep(1:4,times=3))+rnorm(12,0,0.1))
#'
#' saveRDS(conc_data,'conc_data.rds')
#'
#' # sample times
#' build_date <- function(x) as.character(seq(x, length.out=4, by="1 hour"))
#' dates <- unlist(lapply(rep(Sys.time(),3), build_date))
#'
#' samp_data <- data.frame(mod_id = rep(1:3,each=4),
#'                        mod_visit = rep(c(2,1,1),each=4),
#'                        mod_id_visit = as.numeric(paste(rep(1:3,each=4),
#'                                                        rep(c(2,1,1),each=4), sep=".")),
#'                        samp = rep(1:4,times=3),
#'                        Sample.Collection.Date.and.Time = dates)
#' 
#' saveRDS(samp_data,'samp_data.rds')
#' 
#' run_DrugLevel(
#'   conc.path='conc_data.rds',
#'   conc.select=c('mod_id','mod_id_visit','samp','drug_calc_conc'),
#'   conc.rename=c(drug_calc_conc= 'conc.level', samp='event'),
#'   conc.mod.list = list(mod_id_event = expression(paste(mod_id_visit, event, sep = "_"))),
#'   samp.path = 'samp_data.rds',
#'   samp.mod.list = list(mod_id_event = expression(paste(mod_id_visit, samp, sep = "_"))),
#'   check.path = tempdir(),
#'   drugname = 'drugnm',
#'   LLOQ = 0.05
#' )
#'}
#'
#' @export

run_DrugLevel <- function(conc.path, conc.select, conc.rename,
                          conc.mod.list = list(mod_id_event = expression(paste(mod_id_visit, event, sep = '_'))),
                          samp.path = NULL,
                          samp.mod.list = list(mod_id_event = expression(paste(mod_id_visit, samp, sep = '_'))),
                          check.path,
                          failmiss_fn = 'MissingConcDate-',
                          multsets_fn = 'multipleSetsConc-',
                          faildup_fn = 'DuplicateConc-',
                          drugname, LLOQ, demo.list=NULL) {
  #### sample
  if(!is.null(samp.path)) {
    # read and transform data
    samp.in <- readRDS(samp.path)
    samp <- dataTransformation(samp.in, modify = samp.mod.list)
  } else {
    samp <- NULL
  }

  #### concentration

  # read and transform data
  conc.in <- readRDS(conc.path)
  conc <- dataTransformation(conc.in,
    select = conc.select,
    rename = conc.rename,                         
    modify = conc.mod.list
  )

  conc.out <- concData_mod(conc, samp, lowerLimit=LLOQ, drugname=drugname,
                          checkDir = check.path, 
                          dem = demo.list$demo,
                          failmissconc_filename = failmiss_fn,
                          multsets_filename = multsets_fn,
                          faildupconc_filename = faildup_fn)

  return(conc.out)
}
