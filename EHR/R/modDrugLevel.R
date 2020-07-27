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
#' @return drug-level data set
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
