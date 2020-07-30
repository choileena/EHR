#' Run Structured E-Prescription Data
#'
#' This module will load and modify structured e-prescription data.
#'
#' @param file filename of prescription data
#' @param select columns to select
#' @param rename new column names
#'
#' @return str data set
#'
#' @export

run_MedStrII <- function(file,
                        select = c('GRID','MED_NAME','RX_DOSE','FREQUENCY','ENTRY_DATE','STRENGTH_AMOUNT','DESCRIPTION'),
                        rename = c('ID','MED_NAME','RX_DOSE','FREQUENCY','ENTRY_DATE','STRENGTH_AMOUNT','DESCRIPTION')) {
  tac <- readTransform(file, select = select, rename = rename)
  tac_processed <- processErx(tac)
  processErxAddl(tac_processed)
}
