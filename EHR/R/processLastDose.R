#' Process and standardize extracted last dose times
#'
#' This function takes last dose times extracted using the \pkg{medExtractR} system and
#' processes the times into standardized datetime objects using recorded lab data where necessary.
#' The raw output from \code{\link{extractMed}} is filtered to just the LastDose extractions. Time expressions
#' are standardized into HH:MM:SS format based on what category they fall into (e.g., a time represented
#' with AM/PM, 24-hour military time, etc.). When the last dose time is after 12pm, it is assumed to have
#' been taken one day previous to the note's date. For any duration extractions (e.g. "14 hour level"),
#' the last dose time is calculated from the labtime by extracting the appropriate number of hours. The
#' final dataset is returned with last dose time formatted into a POSIXct variable.
#'
#' @param mxrData data.frame containing output from the \code{\link[medExtractR]{medExtractR}} system
#' @param noteMetaData data.frame with meta data (\code{pid} (patient ID) and \code{date}) for
#' the file names contained within \code{mxrData}
#' @param labData data.frame that contains lab dates and times associated with the file names
#' within \code{mxrData}. Must contain columns \code{pid} and \code{date}, as well as \code{labtime}.
#' The \code{date} column must be in the same format as \code{date} in \code{noteMetaData}, and
#' \code{labtime} must be a POSIXct
#'
#' @return data.frame with identifying information (e.g., filename, etc) as well as processed
#' and standardized last dose times as a POSIXct column
#'
#' @examples
#' tac_mxr <- read.csv(system.file("examples", "tac_mxr.csv", package = "EHR"))
#' data(tac_metadata)
#' data(tac_lab)
#'
#' processLastDose(mxrData = tac_mxr, noteMetaData = tac_metadata, labData = tac_lab)
#'
#' @export

processLastDose <- function(mxrData, noteMetaData, labData) {
  
  # CHANGE all_res to mxrLD
  # Restrict to last dose extractions, determine start position as numeric
  mxrLD <- mxrData[mxrData[,'entity'] == 'LastDose',]
  mxrLD[,'ld_start'] <- as.numeric(sub(":.+", "", mxrLD[,'pos']))

  # Standardize the times found to all be in same format
  std_time <- standardizeTime(mxrLD[,'expr'])
  mxrLD[,'std_time'] <- std_time

  # Separate into time vs duration
  ix1 <- checkTime(std_time)
  ix2 <- checkDuration(std_time)
  mxrLD[ix1,'last_dose_time'] <- std_time[ix1]
  mxrLD[ix2,'last_dose_duration'] <- std_time[ix2]

  # Combine with noteMetaData - required to combine with labData
  ix <- match(mxrLD[,'filename'], noteMetaData[,'filename'])
  if(any(is.na(ix))) stop('ensure that all filenames in `mxrData` are present in noteMetaData')
  mxrLD0 <- cbind(mxrLD, noteMetaData[ix, c('pid','date','note')])

  # Left join in case we have extractions but are missing lab vals in this set (so no lab time)
  mxrLD1 <- merge(x = mxrLD0, y = labData, by = c("pid", "date"), all.x = TRUE)

  # Convert duration to datetime based on recorded lab time
  mxrLD1[,'duration_datetime'] <- mxrLD1[,'labtime'] - as.numeric(mxrLD1[,'last_dose_duration'])*60*60

  # For time expressions, check if the hour part is < 12
  ldt_num <- as.numeric(sub("(?<=\\d{2}):\\d{2}:\\d{2}", "", mxrLD1[,'last_dose_time'], perl = TRUE))

  # Create datetime object as character string
  # IF TIME > 12PM, NEED TO SUBTRACT ONE DAY FROM NOTE - we assume PM times are from the previous day
  ld_dt <- ifelse(ldt_num < 12,
                  paste(mxrLD1[,'date'], mxrLD1[,'last_dose_time']),
                  paste(as.Date(mxrLD1[,'date']) - 1, mxrLD1[,'last_dose_time'])
  )
  mxrLD1[,'lastdose_datetime'] <- as.POSIXct(ld_dt, format = '%Y-%m-%d %H:%M:%S')

  no_ld <- is.na(mxrLD1[,'lastdose_datetime'])
  no_dr <- is.na(mxrLD1[,'duration_datetime'])
  if(any(!no_ld & !no_dr)) {
    stop('extraction identified as both time and duration')
  }

  # Mark as time or duration expression - fill in last dose with appropriate value
  is_time <- !no_ld & no_dr
  is_duration <- no_ld & !no_dr

  mxrLD1[,'time_type'] <- NA
  mxrLD1[is_time,'time_type'] <- "time"
  mxrLD1[is_duration,'time_type'] <- "duration"

  mxrLD1[,'lastdose'] <- mxrLD1[,'lastdose_datetime']
  mxrLD1[is_duration,'lastdose'] <- mxrLD1[is_duration,'duration_datetime']

  # Some renaming of variables
  names(mxrLD1)[match(c('expr','pos'), names(mxrLD1))] <- c('raw_time','ld_pos')

  coi <- c("filename", "lastdose", "ld_pos", "pid", "date", "raw_time", "ld_start", "time_type", "labtime")
  mxrLD1[order(mxrLD1[,'filename'], mxrLD1[,'ld_start']), coi]
}
