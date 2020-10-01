#' Create ID Crosswalk
#'
#' Link ID columns from multiple data sets. De-identified columns
#' are created to make a crosswalk.
#'
#' \sQuote{visit.id} and \sQuote{uniq.id} may occur multiple
#' times, but should have a one-to-one linkage defined by at least
#' one of the input data sets. A new visit number is generated for
#' each repeated \sQuote{uniq.id}.
#'
#' @param data list of data.frames
#' @param idcols list of character vectors, indicating ID columns
#' found in each data set given in \sQuote{data}
#' @param visit.id character sting indicating visit-level ID variable (default is "subject_id")
#' @param uniq.id character sting indicating subject-level ID variable (default is "subject_uid")
#'
#' @return crosswalk of ID columns and their de-identified versions
#'
#' @examples
#' \dontrun{
#' demo_data <- data.frame(subj_id=c(4.1,4.2,5.1,6.1),
#'                         pat_id=c(14872,14872,24308,37143),
#'                         gender=c(1,1,0,1),
#'                         weight=c(34,42,28,63),
#'                         height=c(142,148,120,167))
#' 
#' conc_data <- data.frame(subj_id=rep((4:6)+0.1,each=5),
#'                         event=rep(1:5,times=3),
#'                         conc.level=15*exp(-1*rep(1:5,times=3))+rnorm(15,0,0.1))
#' 
#' data <- list(demo_data, conc_data)
#' idcols <- list(c('subj_id', 'pat_id'), 'subj_id')
#' idCrosswalk(data, idcols, visit.id='subj_id', uniq.id='pat_id')
#' 
#' }
#' @export


idCrosswalk <- function(data, idcols, visit.id="subject_id", uniq.id="subject_uid") {
  dl <- length(data)
  il <- length(idcols)
  stopifnot(dl == il)
  res <- vector("list", dl)
  for (i in seq(dl)) {
    df <- data[[i]][, idcols[[i]], drop = FALSE]
    res[[i]] <- unique(df)
  }
  red <- Reduce(function(x, y) merge(x, y, all = TRUE), res)
  # special case: only one ID variable to de-identify
  if(visit.id == uniq.id && ncol(red) == 1 && names(red) == visit.id) {
    red <- red[order(red[, visit.id], red[, uniq.id]),,drop = FALSE]
    uid <- seq(nrow(red))
    red[, "mod_visit"] <- 1
    red[, "mod_id"] <- uid
    red[, "mod_id_visit"] <- paste(uid, red[, "mod_visit"], sep = ".")
  } else {
    ix <- which(is.na(red[, visit.id]))
    red[ix, visit.id] <- ix * -1 + 0.1
    red[, "subj"] <- trunc(red[, visit.id])
    idBySubj <- function(x) unique(x[!is.na(x)])
    meanIds <- tapply(red[, uniq.id], red[, "subj"], idBySubj)
    idlen <- lengths(meanIds)
    meanIds[idlen == 0] <- NA
    meanIds <- unlist(meanIds)
    ix <- which(idlen > 1)
    if (length(ix)) {
      badId <- names(ix)
      print(unique(red[red[, "subj"] %in% badId, c("subj", 
                                                  uniq.id)]))
      stop("discrepancy with visit.id and uniq.id must be fixed")
    }
    red[, uniq.id] <- meanIds[match(red[, "subj"], names(meanIds))]
    red <- red[, c(visit.id, uniq.id)]
    ix <- which(is.na(red[, uniq.id]))
    red[ix, uniq.id] <- ix * -100
    red <- unique(red)
    red <- red[order(red[, visit.id], red[, uniq.id]),]
    dd <- duplicated(red[, uniq.id])
    ddcs <- cumsum(dd)
    ddncs <- cumsum(!dd)
    red[, "mod_visit"] <- ddcs - ddcs[which(!dd)][ddncs] + 1
    red[, "mod_id"] <- ddncs
    red[, "mod_id_visit"] <- paste(ddncs, red[, "mod_visit"], 
                                  sep = ".")
    red[red[, visit.id] <= 0, visit.id] <- NA
    red[red[, uniq.id] <= 0, uniq.id] <- NA
  }
  rownames(red) <- NULL
  attr(red, "deidentified_cols") <- c("mod_visit", "mod_id", 
                                      "mod_id_visit")
  red
}


#' Pull Fake/Mod ID
#'
#' Replace IDs with de-identified version pulled from a crosswalk.
#'
#' @param dat a data.frame
#' @param xwalk a data.frame providing linkage for each ID, e.g. output from \code{\link{idCrosswalk}}
#' @param firstCols name of columns to put at front of output data set
#' @param orderBy name of columns used to reorder output data set
#' @param uniq.id character string indicating subject-level id variable (default is "subject_uid")
#'
#' @return The modified data.frame
#' 
#' @examples 
#' \dontrun{
#' demo_data <- data.frame(subj_id=c(4.1,4.2,5.1,6.1),
#'                         pat_id=c(14872,14872,24308,37143),
#'                         gender=c(1,1,0,1),
#'                         weight=c(34,42,28,63),
#'                         height=c(142,148,120,167))
#' 
#' # crosswalk w/ same format as idCrosswalk() output
#' xwalk <- data.frame(subj_id=c(4.1,4.2,5.1,6.1),
#'                     pat_id=c(14872,14872,24308,37143),
#'                     mod_visit=c(1,2,1,1),
#'                     mod_id=c(1,1,2,3),
#'                     mod_id_visit=c(1.1,1.2,2.1,3.1))
#' 
#' demo_data_deident <- pullFakeId(demo_data, xwalk, 
#'                                 firstCols = c('mod_id','mod_id_visit','mod_visit'),
#'                                 uniq.id='pat_id')
#' }
#' 
#' 
#' @export

pullFakeId <- function(dat, xwalk, firstCols = NULL, orderBy = NULL, uniq.id="subject_uid") {
  cmn <- intersect(names(dat), names(xwalk))
  # uniq.id (MRN) may have multiple IDs; if it's the key, reduce
  if(length(cmn) == 1 && cmn == uniq.id) {
    xwalk <- unique(xwalk[, c(uniq.id,'mod_id')])
    xwalk <- xwalk[!is.na(xwalk[,uniq.id]),]
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
  x <- x[,-match(toremove, names(x)), drop = FALSE]
  if(is.null(orderBy)) {
    x <- x[order(x[,'row_order']),-match('row_order', names(x)), drop = FALSE]
  } else {
    x <- x[do.call(order, as.list(x[,orderBy,drop = FALSE])),, drop = FALSE]
  }
  rownames(x) <- NULL
  if(!is.null(firstCols)) {
    x <- x[,c(firstCols, setdiff(names(x), firstCols)), drop = FALSE]
  }
  x
}


#' Pull Real ID
#'
#' Replace de-identified IDs with identified version pulled from a crosswalk.
#'
#' @param dat a data.frame
#' @param xwalk a data.frame providing linkage for each ID, e.g. output from \code{\link{idCrosswalk}}; 
#' if NULL, the crosswalk will be pulled from the \sQuote{pkxwalk} option, or otherwise the
#' unmodified data.frame.
#' @param remove.mod.id logical, should the de-identified IDs -- mod_id, mod_visit, mod_id_visit -- 
#' be removed (default=FALSE)
#'
#' @return The modified data.frame
#' 
#' @examples 
#' \dontrun{
#' demo_data_deident <- data.frame(mod_id=c(1,1,2,3),
#'                                 mod_id_visit=c(1.1,1.2,2.1,3.1),
#'                                 mod_visit=c(1,2,1,1),
#'                                 gender=c(1,1,0,1),
#'                                 weight=c(34,42,28,63),
#'                                 height=c(142,148,120,167))
#' 
#' # crosswalk w/ same format as idCrosswalk() output
#' xwalk <- data.frame(subj_id=c(4.1,4.2,5.1,6.1),
#'                     pat_id=c(14872,14872,24308,37143),
#'                     mod_visit=c(1,2,1,1),
#'                     mod_id=c(1,1,2,3),
#'                     mod_id_visit=c(1.1,1.2,2.1,3.1))
#' 
#' pullRealId(demo_data_deident, xwalk)
#' pullRealId(demo_data_deident, xwalk, remove.mod.id=TRUE)
#' }
#' 
#' 
#' @export

pullRealId <- function(dat, xwalk = NULL, remove.mod.id=FALSE) {
  if(is.null(xwalk)) {
    xwalkVar <- getOption('pkxwalk')
    if(!is.null(xwalkVar) && xwalkVar %in% ls(envir = .GlobalEnv)) {
      xwalk <- get(xwalkVar, envir = .GlobalEnv)
    } else {
      return(dat)
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
    dat <- cbind(xw[ix,,drop = FALSE], dat)
  }
  
  if(remove.mod.id){ 
    dat <- dat[,!grepl('^mod_',names(dat))]
  }
dat
}
