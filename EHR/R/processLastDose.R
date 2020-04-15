#' Process and standardize extracted last dose times
#'
#' This function takes last dose times extracted using the \code{medExtractR} system and 
#' processes the times into standardized datetime objects using recorded lab data.
#'
#' @param mxrData data.frame containing output from the \code{medExtractR} system
#' @param noteMetaData data.frame with meta data (\code{pid} (patient ID) and \code{date}) for 
#' the file names contained within \code{mxrData}
#' @param labData data.frame that contains lab dates and times associated with the file names 
#' within \code{mxrData}. Must contain columns \code{pid} and \code{date}, as well as \code{labtime}. 
#' The \code{date} column must be in the same format as \code{date} in \code{noteMetaData}, and 
#' \code{labtime} must be a POSIXct 
#'
#' @return data.frame with identifying information (e.g., filename, etc) as well as processed 
#' and standardized last dose times as a POSIXct column
#' @export
#'
#' @examples
#' ## Lastdose post processing - just time standardization
#' tac_wd <- file.path('/', 'Volumes', 'pk', 'tacrolimus')
#' ld_wd <- file.path('/', 'Volumes', 'pk', 'tacrolimus', 'lastdose_pk')
#' pp_wd <- file.path('/', 'Volumes', 'pk', 'postprocess', 'validation', 'lastdose')
#' 
#' # read in the dataset being used to test lastdose
#' #df <- read.csv(file.path(pp_wd, 'mxr_tac_notes.csv')) # all extracted data - lastdose rows removed (leaving this way, cole might need LD and non-LD info separate)
#' df <- read.csv(file.path(pp_wd, 'mxr_tac_lastdose_info.csv')) # last dose extractions
#' 
#' id_xwalk <- read.csv(file.path(tac_wd, 'tacGRIDlabtime.csv'),
#'                      stringsAsFactors = FALSE) %>%
#'   select(OLD_RUID, GRID, labtime)
#' tac_data <- read.csv(file.path(tac_wd, 'dmet', 'finalLDAdata446.csv'),
#'                      stringsAsFactors = FALSE) %>%
#'   select(c(id, labtime, conc, dose_morn, dose_even))
#' # convert labtime to datetime variable
#' id_xwalk <- within(id_xwalk, labtime <- as.POSIXct(labtime, format = '%m/%d/%y %H:%M'))
#' tac_data <- within(tac_data, labtime <- as.POSIXct(labtime, format = '%m/%d/%y %H:%M'))
#' tac_lab <- left_join(tac_data, id_xwalk, by = c("id" = "OLD_RUID", "labtime")) %>%
#'   select(-id) %>% arrange(GRID, labtime) %>% select(GRID, labtime, conc, dose_morn, dose_even) %>%
#'   mutate(lab_date = str_extract(labtime, "\\d{4}-\\d{2}-\\d{2}"))
#' 
#' tac_lab$pid <- tac_lab$GRID
#' tac_lab$date <- tac_lab$lab_date
#' 
#' # data frame with the note meta data
# meta_data <- data.frame(filename = df$filename)
# meta_data$pid <- sapply(df$filename, function(x) strsplit(x, "_")[[1]][1])
# meta_data$date <- sapply(df$filename, function(x) strsplit(x, "_")[[1]][2])
# meta_data$note <- sub(".txt", "", sapply(df$filename, function(x) strsplit(x, "_")[[1]][3]), fixed=TRUE)
#' 
#' processLastDose(mxrData = df, noteMetaData = meta_data, labData = tac_lab)
#' 


processLastDose <- function(mxrData, noteMetaData, labData){
  
  # CHANGE all_res to mxrLD
  # Restrict to last dose extractions, determine start position as numeric
  mxrLD <- subset(mxrData, entity == "LastDose")
  mxrLD$ld_start <- as.numeric(sub(":.+", "", mxrLD$pos))
  
  # Standardize the times found to all be in same format
  mxrLD$std_time <- standardizeTime(mxrLD$expr)
  
  # Separate into time vs duration
  mxrLD$last_dose_time <- ifelse(sapply(mxrLD$std_time, checkTime),
                                 mxrLD$std_time, NA)
  mxrLD$last_dose_duration <- ifelse(sapply(mxrLD$std_time, checkDuration),
                                       mxrLD$std_time, NA)

  # Combine with noteMetaData - required to combine with labData
  mxrLD0 <- merge(x = mxrLD, y = noteMetaData, by = c("filename"))
  
  
  ## NEED TO REPLACE DPLYR CODE WITH BASE, all_res change to mxrLD
  # Left join in case we have extractions but are missing lab vals in this set (so no lab time)
  mxrLD1 <- merge(x = mxrLD0, y = labData, 
                  by = c("pid", "date"), , all.x = TRUE) 
  
  
  # Convert duration to datetime based on recorded lab time
  mxrLD1$duration_datetime <- mxrLD1$labtime - as.numeric(mxrLD1$last_dose_duration)*60*60
  
  # For time expressions, check if the hour part is < 12
  hr_part <- ifelse(as.numeric(sub("(?<=\\d{2}):\\d{2}:\\d{2}", "", 
                                   mxrLD1$last_dose_time, perl=T)) < 12,
                    TRUE, FALSE)
  # Create datetime object as character string
  # IF TIME > 12PM, NEED TO SUBTRACT ONE DAY FROM NOTE - we assume PM times are from the previous day
  ld_dt <- ifelse(hr_part,
                  paste(mxrLD1$date, mxrLD1$last_dose_time),
                  paste(as.Date(mxrLD1$date) - 
                          as.difftime(1, unit="days"), mxrLD1$last_dose_time))
  
  mxrLD1$lastdose_datetime <- as.POSIXct(ld_dt, format = '%Y-%m-%d %H:%M:%S')

  if(any(!is.na(mxrLD1$lastdose_datetime) & !is.na(mxrLD1$duration_datetime))){
    stop('extraction identified as both time and duration')
  }
  
  # Mark as time or duration expression - fill in last dose with appropriate value
  is_time <- !is.na(mxrLD1$lastdose_datetime) & is.na(mxrLD1$duration_datetime)
  is_duration <- is.na(mxrLD1$lastdose_datetime) & !is.na(mxrLD1$duration_datetime)
  
  mxrLD1$time_type <- NA
  mxrLD1$time_type[is_time] <- "time"
  mxrLD1$time_type[is_duration] <- "duration"
  
  mxrLD1$lastdose <- mxrLD1$lastdose_datetime
  mxrLD1$lastdose[is_duration] <- subset(mxrLD1$duration_datetime, is_duration)
  

  # Some renaming of variables
  mxrLD1$raw_time <- mxrLD1$expr
  mxrLD1$ld_pos <- mxrLD1$pos
  
  ld_std_output <- mxrLD1[,c("filename", "lastdose", "ld_pos", "pid", "date", 
                             "raw_time", "ld_start", "time_type", "labtime")]
  
  ld_std_output <- ld_std_output[order(ld_std_output$filename,
                                       ld_std_output$ld_start),]
  
  return(ld_std_output)
}


