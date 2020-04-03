#' Internal functions for lastdose
#'
#' These internal functions incorporate lastdose.
#' 
#' \code{matchLastDose}: match lastdose data to built data
#' 
#' \code{mergeLastDose}: merge lastdose data into built data
#'
#' @name lastdose-internal
#' @aliases matchLastDose mergeLastDose
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

#' Add Lastdose Data
#'
#' Add lastdose data to data set
#'
#' Lastdose is a datetime string associated with dose data.
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
