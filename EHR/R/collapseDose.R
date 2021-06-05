#' Collapse Dose Data
#'
#' Splits drug data and calls \code{\link{makeDose}} to collapse at the note and date level.
#'
#' If different formulations of the drug (e.g., extended release) exist, they can be
#' separated using a regular expression (e.g., \sQuote{xr|er}). This function will call
#' \code{\link{makeDose}} on parsed and paired medication data to calculate dose intake
#' and daily dose and remove redundancies at the note and date level.
#' 
#' See EHR Vignette for Extract-Med and Pro-Med-NLP as well as Dose Building Using Example Vanderbilt EHR Data for details.
#' 
#' @param x data.frame containing the output of \code{\link{buildDose}}, or the output of
#' \code{\link{addLastDose}} if last dose information is being incorporated.
#' @param noteMetaData data.frame containing identifying meta data for each
#' note, including patient ID, date of the note, and note ID. Column names
#' should be set to \sQuote{filename}, \sQuote{pid}, \sQuote{date},
#' \sQuote{note}. Date should have format YYYY-MM-DD.
#' @param naFreq Expression used to replace missing frequencies with, or by default use the most
#' common.
#' @param \dots drug formulations to split by
#'
#' @return A list containing two dataframes, one with the note level and one with the date level collapsed data.
#' 
#' @examples
#' data(lam_mxr_parsed)
#' data(lam_metadata)
#' 
#' lam_build_out <- buildDose(lam_mxr_parsed)
#' 
#' lam_collapsed <- collapseDose(lam_build_out, lam_metadata, naFreq = 'most', 'xr|er')
#' lam_collapsed$note # Note level collapsing
#' lam_collapsed$date # Date level collapsing
#' @export

collapseDose <- function(x, noteMetaData, naFreq = 'most', ...) {
  ld <- list(...)
  if(length(ld)) {
    dn <- x[['drugname']]
    path <- numeric(length(dn))
    for(i in seq_along(ld)) {
      path[grepl(ld[[i]], dn, ignore.case = TRUE) & path == 0] <- i
    }
    sx <- split(x, path)
    res <- lapply(sx, makeDose, noteMetaData, naFreq)
    rn <- do.call(qrbind, lapply(res, function(i) i[['note']]))
    rn <- rn[order(rn[[1]]),]
    rownames(rn) <- NULL
    rd <- do.call(qrbind, lapply(res, function(i) i[['date']]))
    rd <- rd[order(rd[[1]]),]
    rownames(rd) <- NULL
    nx <- list(note = rn, date = rd)
  } else {
    nx <- makeDose(x, noteMetaData, naFreq)
  }
  nx
}
