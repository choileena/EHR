#' Run Structured E-Prescription Data
#'
#' This module will load and modify structured e-prescription data.
#'
#' @param file filename of prescription data (stored as CSV)
#' @param select columns to select
#' @param rename new column names
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return str data set
#'
#' @examples
#' \dontrun{
#' erx_data <- data.frame(GRID=paste0("ID",c(1,1,2,2,2,2)),
#'                        MED_NAME=c("fakedrug","fakedrug","fakedrug",
#'                                   "Brandname","fakedrug","fakedrug"),
#'                        RX_DOSE=c(1,2,1,'2 tabs',1,'1+1.5+1'),
#'                        FREQUENCY=c(rep("bid",3),"qam","bid",
#'                                    "brkfst,lunch,dinner"),
#'                        ENTRY_DATE=c("2018-02-15","2018-03-14","2017-07-01",
#'                                     "2017-07-01","2017-09-15","2017-11-01"),
#'                        STRENGTH_AMOUNT=c("100","100","200",
#'                                          "100mg","100","100"),
#'                        DESCRIPTION=c("fakedrug 100 mg tablet","fakedrug 100 mg tablet",
#'                                      "fakedrug 200 mg tablet (also known as brandname)",
#'                                      "Brandname 100mg tablet", "fakedrug 100 mg tablet",
#'                                      "fakedrug 100 mg tablet"))
#' 
#' write.csv(erx_data, 'erx_data.csv') 
#' 
#' run_MedStrII('erx_data.csv')                   
#' }
#'
#'
#' @export

run_MedStrII <- function(file,
                        select = c('GRID','MED_NAME','RX_DOSE','FREQUENCY','ENTRY_DATE','STRENGTH_AMOUNT','DESCRIPTION'),
                        rename = c('ID','MED_NAME','RX_DOSE','FREQUENCY','ENTRY_DATE','STRENGTH_AMOUNT','DESCRIPTION')) {
  tac <- readTransform(file, select = select, rename = rename)
  tac_processed <- processErx(tac)
  processErxAddl(tac_processed)
}
