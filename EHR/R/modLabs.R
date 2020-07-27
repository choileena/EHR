#' Run Lab Data
#'
#' This module will load and modify laboratory data.
#'
#' @param lab.path filename of a lab file (stored as RDS)
#' @param lab.select columns to select
#' @param lab.mod.list list of expressions, giving modifications to make
#'
#' @return lab data set
#'
#' @export

run_Labs <- function(lab.path, lab.select, lab.mod.list) {
# # read and transform data
  lab.in <- readRDS(lab.path)
#   lab.in <- read.csv(lab.path)
#   # convert MRN into mod_id
#   xx <- get(getOption('pkxwalk'), .GlobalEnv)
#   lab.in[,'mod_id'] <- xx[match(lab.in[,'Subject.mrn'], xx[,'mrn']), 'mod_id']
  lab <- dataTransformation(lab.in, modify = lab.mod.list)
  lab[,lab.select]
}
