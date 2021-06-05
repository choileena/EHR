#' Add Lastdose Data
#'
#' Add lastdose data to data set from the \code{\link{buildDose}} process.
#'
#' Lastdose is a datetime string associated with dose data. Information on time
#' of last dose can be extracted within the \code{\link{extractMed}} function
#' (i.e., \code{\link[medExtractR]{medExtractR}}) using the argument
#' \code{lastdose=TRUE}. Raw extracted times should first be processed using the
#' \code{\link{processLastDose}} function to convert to datetime format before providing
#' to \code{addLastDose}. This function then combines the processed last dose times with output
#' from the \code{\link{buildDose}} process by file name to pair last dose times with dosing regimens based on position.
#' Alternatively, the user can provide their own table of lastdose data. In this case, with position
#' information absent, the lastdose data should be restricted to one unique last dose time per
#' unique patient ID-date identifier.
#'
#' In the case where \code{lastdoseData} is output from \code{\link{processLastDose}}, it is possible to
#' have more than one extracted last dose time. In this case, rules are applied to determine which
#' time should be kept. First, we give preference to an explicit time expression (e.g., "10:30pm")
#' over a duration expression (e.g., "14 hour level"). Then, we pair last dose times with drug regimens
#' based on minimum distance between last dose time start position and drug name start position.
#' 
#' See EHR Vignette for Extract-Med and Pro-Med-NLP for details.
#' 
#' @param buildData data.frame, output of \code{\link{buildDose}} function.
#' @param lastdoseData data.frame with columns filename, ld_start, lastdose, raw_time, time_type
#'
#' @return a data.frame with the \sQuote{lastdose} column added.
#' @examples
#' # Get build data
#' data(tac_mxr_parsed)
#' # don't combine lastdose at this stage
#' tac_build <- buildDose(tac_mxr_parsed, preserve = 'lastdose')
#' # Get processed last dose data
#' tac_mxr <- read.csv(system.file("examples", "tac_mxr.csv", package = "EHR"))
#' data(tac_metadata)
#' data(tac_lab)
#' ld_data <- processLastDose(tac_mxr, tac_metadata, tac_lab)
#'
#' addLastDose(tac_build, ld_data)
#'
#' @export
addLastDose <- function(buildData, lastdoseData) {
  mld <- mergeLastDose(buildData, lastdoseData)
  mld <- mld[!is.na(mld[,'lastdose']),]
  key1 <- do.call(paste, c(buildData[,c('filename','drugname_start')], sep = '|'))
  key2 <- do.call(paste, c(mld[,c('filename','drugname_start')], sep = '|'))
  ix <- match(key1, key2)
  buildData[,'lastdose'] <- mld[ix,'lastdose']
  buildData
}
