#' Internal functions for lastdose
#'
#' These internal functions incorporate lastdose.
#' 
#' \code{matchLastDose}: match lastdose data to built data
#' 
#' \code{mergeLastDose}: merge lastdose data into built data
#' 
#' \code{getDuration}: Duration - don't convert just extract number
#'
#' \code{convertAMPM}: convert am/pm times
#'
#' \code{convertMornNight}: if night/morning used, convert to am or pm
#'
#' \code{convertMilitary}: convert military format into 12h
#'
#' \code{standardizeTime}: create standardized form of time
#' 
#' \code{checkTime}: check if lastdose expression is a time expression in the format HH:MM:SS
#' 
#' \code{checkDuration}: check if lastdose expression is a duration expression (number that is
#' not in the HH:MM:SS format)
#'
#' @name lastdose-internal
#' @aliases matchLastDose mergeLastDose checkTime checkDuration standardizeTime
#' convertMilitary convertMornNight convertAMPM getDuration
#' @keywords internal
NULL

matchLastDose <- function(x, y) {
  # x was created with EHR::buildDose
  xnr <- nrow(x)
  ynr <- nrow(y)
  dist <- abs(vapply(y[,'ld_start'], `-`, numeric(xnr), x[,'drugname_start']))
  opt <- y
  coi <- c('lastdose','raw_time','time_type')
  if(ynr > 1) {
    ix <- which(y[,'time_type'] == 'time')
    if(length(ix) != 0) {
      y <- y[ix,]
    }
    if(nrow(y) > 1) {
      ix <- ((which.min(dist) - 1) %/% xnr) + 1
      y <- y[ix,]
    }
    ix <- which(opt[,'ld_start'] != y[,'ld_start'])
    opt[ix,coi] <- y[,coi]
  }
  if(xnr == 1) {
    mtch <- rep(1, ynr)
  } else {
    mtch <- apply(dist, 2, which.min)
  }
  coi <- c('ld_start', coi)
  out <- x[,c('filename','drugname_start')]
  out[mtch,coi] <- opt[,coi]
  out
}

mergeLastDose <- function(buildData, lastdoseData) {
  bd <- unique(buildData[,c('filename','drugname_start')])
  tbl <- split(bd, bd[,'filename'])
  ldl <- split(lastdoseData, lastdoseData[,'filename'])
  ids <- names(ldl)
  # qrbind is faster but fails with POSIXct
  do.call(rbind, lapply(ids, function(i) matchLastDose(tbl[[i]], ldl[[i]])))
}

convertMilitary <- function(tm) {
  ix <- which(!is.na(tm) & !grepl("[a-z]", tm))
  if(length(ix)) {
    val <- tm[ix]
    if(any(nchar(val) != 4)) stop("unexpected time length")
    val <- sprintf("%s:%s:00", substr(val, 1, 2), substr(val, 3, 4))
    tm[ix] <- val
  }
  tm
}

convertMornNight <- function(tm) {
  ix <- which(!is.na(tm) & grepl("night", tm))
  if(length(ix)) {
    tm[ix] <- paste0(gsub("[a-z]", "", tm[ix]), "pm")
  }
  ix <- which(!is.na(tm) & grepl("morn", tm))
  if(length(ix)) {
    tm[ix] <- paste0(gsub("[a-z]", "", tm[ix]), "am")
  }
  tm
}

convertAMPM <- function(tm) {
  ix <- which(!is.na(tm) & grepl("[0-9](a|p)m?$", tm))
  if(length(ix)) {
    val <- tm[ix]
    is_pm <- grepl("[0-9]pm?$", val) & !grepl("^12", val)
    num <- gsub('^([0-9]+)(a|p)m?$', '\\1', val)
    ndig <- nchar(num)
    ix1 <- ndig %in% c(1,2)
    ix3 <- ndig %in% c(3,4)
    num[ix1] <- sprintf("%02d:00:00", as.numeric(num[ix1]) + 12 * is_pm[ix1])
    hourpart <- as.numeric(substr(num[ix3], 1, ndig[ix3] - 2)) + 12 * is_pm[ix3]
    mintpart <- substr(num[ix3], ndig[ix3] - 1, ndig[ix3])
    num[ix3] <- sprintf("%02d:%s:00", hourpart, mintpart)
    # Not a real time
    num[as.numeric(substr(num, 1, 2)) > 23] <- NA
    tm[ix] <- num
  }
  tm
}

getDuration <- function(tm) {
  ix <- which(!is.na(tm) & grepl("hr|hours?", tm))
  if(length(ix)) {
    val <- gsub("(hr|hour)s?", "", tm[ix])
    ix1 <- which(nchar(gsub("[^0-9]", "", val)) == 4 & !grepl("[.]", val))
    if(length(ix1)) {
      val[ix1] <- convertMilitary(val[ix1])
    }
    tm[ix] <- val
  }
  tm
}

standardizeTime <- function(time_string) {
  time_string <- gsub("\n| |:", "", tolower(time_string))
  orig <- time_string
  time_string <- convertMilitary(time_string)
  time_string <- convertMornNight(time_string)
  time_string <- convertAMPM(time_string)
  time_string <- getDuration(time_string)
  names(time_string) <- orig
  time_string
}

# distinguish between time and duration
checkTime <- function(tm){
  !is.na(tm) & nchar(tm) == 8 &
    substr(tm, 3, 3) == ":" & substr(tm, 6, 6) == ":"
}

checkDuration <- function(tm){
  !is.na(tm) & !checkTime(tm)
}
