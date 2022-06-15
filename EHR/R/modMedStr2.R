#' Run Structured E-Prescription Data
#'
#' This module will load and modify structured e-prescription data.
#'
#' @param file filename of prescription data (CSV, RData, RDS), or data.frame
#' @param dat.columns a named list that should specify columns in data; \sQuote{id},
#' \sQuote{dose}, \sQuote{freq}, \sQuote{date}, and \sQuote{str} are required.
#' \sQuote{desc} may also be specified.
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return str data set
#'
#' @examples
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
#' run_MedStrII(erx_data, list(id = 'GRID', dose = 'RX_DOSE', freq = 'FREQUENCY',
#'              date = 'ENTRY_DATE', str = 'STRENGTH_AMOUNT', desc = 'DESCRIPTION'))
#'
#' @export

run_MedStrII <- function(file, dat.columns = list()) {
  x <- read(file)
  req <- list(id = NA, dose = NA, freq = NA, date = NA, str = NA, desc = NULL)
  col <- validateColumns(x, dat.columns, req)
  xDT <- x[,col$date]
  keepED <- col$date == 'ENTRY_DATE'
  x[,'ENTRY_DATE'] <- pkdata::parse_dates(xDT)
  col$date <- 'ENTRY_DATE'
  colix <- match(col, names(x))
  orig <- names(x)[colix]
  hasDesc <- 'desc' %in% names(col)
  if(hasDesc) {
    cnames <- c('ID','RX_DOSE','FREQUENCY','ENTRY_DATE','STRENGTH_AMOUNT','DESCRIPTION')
  } else {
    cnames <- c('ID','RX_DOSE','FREQUENCY','ENTRY_DATE','STRENGTH_AMOUNT')
  }
  # replace columns names with hard-coded expected values
  names(x)[colix] <- cnames
  step1 <- processErx(x, description = hasDesc)
  step2 <- processErxAddl(step1)
  # restore original column names
  names(step2)[colix] <- orig
  if(!keepED) {
    step2[,'ENTRY_DATE'] <- NULL
  }
  step2
}
