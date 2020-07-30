#' Create ID Crosswalk
#'
#' Link ID columns from multiple data sets. Currently restricted to
#' \sQuote{subject_id} and \sQuote{subject_mrn}. De-identified columns
#' are created to make a crosswalk.
#'
#' \sQuote{subject_id} and \sQuote{subject_mrn} may occur multiple
#' times, but should have a one-to-one linkage defined by at least
#' one of the input data sets. A new visit number is generated for
#' each repeated \sQuote{subject_mrn}.
#'
#' @param data list of data.frames
#' @param idcols list of character vectors, indicating ID columns
#' found in each data set given in \sQuote{data}
#'
#' @return crosswalk of ID columns and their de-identified versions
#'
#' @examples
#' \donttest{
#' data <-  list(demo_data, conc_data)
#' idcols <-  list(c('subject_id', 'subject_mrn'), 'subject_id')
#' idCrosswalk(data, idcols)
#' }
#' @export

idCrosswalk <- function(data, idcols) {
  dl <- length(data)
  il <- length(idcols)
  stopifnot(dl == il)
  res <- vector('list', dl)
  for(i in seq(dl)) {
    df <- data[[i]][,idcols[[i]], drop = FALSE]
    res[[i]] <- unique(df)
  }
  red <- Reduce(function(x, y) merge(x, y, all = TRUE), res)
  ix <- which(is.na(red[,'subject_id']))
  red[ix,'subject_id'] <- ix * -1 + 0.1
  red[,'subj'] <- trunc(red[,'subject_id'])
  idBySubj <- function(x) unique(x[!is.na(x)])
  # some MRNs are missing, but can be recovered by subj
  meanIds <- tapply(red[,'subject_mrn'], red[,'subj'], idBySubj)
  idlen <- lengths(meanIds)
  meanIds[idlen == 0] <- NA
  meanIds <- unlist(meanIds)
  ix <- which(idlen > 1)
  if(length(ix)) {
    badId <- names(ix)
    print(unique(red[red[,'subj'] %in% badId,c('subj', 'subject_mrn')]))
    stop('discrepancy with subject_id and subject_mrn must be fixed')
  }
  red[,'subject_mrn'] <- meanIds[match(red[,'subj'], names(meanIds))]
  red <- red[,c('subject_id', 'subject_mrn')]
  ix <- which(is.na(red[,'subject_mrn']))
  red[ix,'subject_mrn'] <- ix * -100.0
  red <- unique(red)
  red <- red[order(red[,'subject_id'], red[,'subject_mrn']),]
  dd <- duplicated(red[,'subject_mrn'])
  ddcs <- cumsum(dd)
  ddncs <- cumsum(!dd)
  red[,'mod_visit'] <- ddcs - ddcs[which(!dd)][ddncs] + 1
  red[,'mod_id'] <- ddncs
  red[,'mod_id_visit'] <- paste(ddncs, red[,'mod_visit'], sep = '.')
  red[red[,'subject_id'] <= 0, 'subject_id'] <- NA
  red[red[,'subject_mrn'] <= 0, 'subject_mrn'] <- NA
  rownames(red) <- NULL
  attr(red, 'deidentified_cols') <- c('mod_visit', 'mod_id', 'mod_id_visit')
  red
}

#' Pull Fake/Mod ID
#'
#' Replace IDs with de-identified version pulled from a crosswalk.
#'
#' @param dat a data.frame
#' @param xwalk a data.frame, providing linkage for each ID
#' @param firstCols name of columns to put at front of output data set
#' @param orderBy name of columns used to reorder data set
#'
#' @return The modified data.frame
#' 
#' @export

pullFakeId <- function(dat, xwalk, firstCols = NULL, orderBy = NULL) {
  cmn <- intersect(names(dat), names(xwalk))
  # MRN may have multiple IDs; if it's the key, reduce
  if(length(cmn) == 1 && cmn == 'subject_mrn') {
    xwalk <- unique(xwalk[, c('subject_mrn','mod_id')])
    xwalk <- xwalk[!is.na(xwalk[,'subject_mrn']),]
  }
  if('deidentified_cols' %in% names(attributes(xwalk))) {
    toremove <- setdiff(names(xwalk), attr(xwalk, 'deidentified_cols'))
  } else {
    toremove <- cmn
  }
  if(is.null(orderBy)) {
    dat[,'row_order'] <- seq(nrow(dat))
  }
  x <- merge(dat, xwalk, by = cmn, all.x = TRUE)
  x <- x[,-match(toremove, names(x))]
  if(is.null(orderBy)) {
    x <- x[order(x[,'row_order']),-match('row_order', names(x))]
  } else {
    x <- x[do.call(order, as.list(x[,orderBy,drop = FALSE])),]
  }
  rownames(x) <- NULL
  if(!is.null(firstCols)) {
    x <- x[,c(firstCols, setdiff(names(x), firstCols))]
  }
  x
}

#' Pull Real ID
#'
#' Replace de-identified IDs with identified version pulled from a crosswalk.
#'
#' @param dat a data.frame
#' @param xwalk a data.frame, providing linkage for each ID; if NULL, the
#' crosswalk will be pulled from the \sQuote{pkxwalk} option, or otherwise fail
#'
#' @return The modified data.frame
#' 
#' @export

pullRealId <- function(dat, xwalk = NULL) {
  if(is.null(xwalk)) {
    xwalkVar <- getOption('pkxwalk')
    if(!is.null(xwalkVar) && xwalkVar %in% ls(envir = .GlobalEnv)) {
      xwalk <- get(xwalkVar, envir = .GlobalEnv)
    } else {
      stop('xwalk must be provided')
    }
  }
  xnames <- names(xwalk)
  dnames <- names(dat)
  coi <- c(xnames[!grepl('^mod_', xnames)])
  xw <- xwalk[,coi,drop = FALSE]
  ix <- FALSE
  if('mod_id_event' %in% dnames && 'mod_id_visit' %in% xnames) {
    ix <- match(sub('_.*', '', dat[,'mod_id_event']), xwalk[,'mod_id_visit'])
  } else if('mod_id_visit' %in% dnames && 'mod_id_visit' %in% xnames) {
    ix <- match(dat[,'mod_id_visit'], xwalk[,'mod_id_visit'])
  } else if('mod_id' %in% dnames && 'mod_id' %in% xnames) {
    ix <- match(dat[,'mod_id'], xwalk[,'mod_id'])
  }
  if(length(ix) == nrow(dat)) {
    dat <- cbind(xw[ix,], dat)
  }
  dat
}
