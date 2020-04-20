#' Add Lastdose Data
#'
#' Add lastdose data to data set from the \code{build} process.
#'
#' Lastdose is a datetime string associated with dose data. Information on time 
#' of last dose can be extracted within the \code{extractMed} function (i.e., \code{medExtractR}) 
#' using the argument \code{lastdose=TRUE}. Raw extracted times should first be processed 
#' using the \code{processLastDose} function to convert to datetime format before providing 
#' to \code{addLastDose}. This function then combines the processed last dose times with output 
#' from the \code{build} process by file name to pair last dose times with dosing regimens based on position. 
#' Alternatively, the user can provide their own table of lastdose data. In this case, with position 
#' information absent, the lastdose data should be restricted to one unique last dose time per 
#' unique patient ID-date identifier.  
#' 
#' In the case where \code{lastdoseData} is output from \code{processLastDose}, it is possbile to 
#' have more than one extracted last dose time. In this case, rules are applied to determine which 
#' time should be kept. First, we give preference to an explicit time expression (e.g., "10:30pm") 
#' over a duration expression (e.g., "14 hour level"). Then, we pair last dose times with drug regimens 
#' based on minimum distance between last dose time start position and drug name start position. 
#'
#' @param buildData data.frame, output of \code{build} function.
#' @param lastdoseData data.frame with columns filename, ld_start, lastdose,
#' raw_time, time_type
#'
#' @return a data.frame with the \sQuote{lastdose} column added.
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