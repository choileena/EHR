#' Internal functions for lastdose
#'
#' These internal functions incorporate lastdose.
#' 
#' \code{matchLastDose}: match lastdose data to built data
#' 
#' \code{mergeLastDose}: merge lastdose data into built data
#' 
#' \code{checkTime}: check if lastdose expression is a time expression in the format HH:MM:SS
#' 
#' \code{checkDuration}: check if lastdose expression is a duration expression (number that is 
#' not in the HH:MM:SS format)
#'
#' @name lastdose-internal
#' @aliases matchLastDose mergeLastDose checkTime checkDuration
#' @keywords internal
NULL

matchLastDose <- function(x, y) {
  # x was created with EHR::build
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
  out <- x[,c('filename','drugname_start')]
  coi <- c('ld_start', coi)
  out[,coi] <- NA
  out[mtch,coi] <- opt[,coi]
  out
}

mergeLastDose <- function(buildData, lastdoseData) {
  bd <- unique(buildData[,c('filename','drugname_start')])
  tbl <- split(bd, bd[,'filename'])
  ldl <- split(lastdoseData, lastdoseData[,'filename'])
  ids <- names(ldl)
  do.call(qrbind, lapply(ids, function(i) matchLastDose(tbl[[i]], ldl[[i]])))
}

# distinguish between time and duration
checkTime <- function(tm){
  !is.na(tm) & nchar(tm) == 8 &
    substr(tm, 3, 3) == ":" & substr(tm, 6, 6) == ":"
}

checkDuration <- function(tm){
  !is.na(tm) & !checkTime(tm)
}
