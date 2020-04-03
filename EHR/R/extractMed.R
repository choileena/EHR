#' Extract medication inforamtion from clinical notes
#'
#' This function is an interface to the \code{medExtractR} function within the \code{medExtractR} 
#' package, and allows drug dosing information to be extracted from free text sources, 
#' e.g., clinical notes.
#' 
#' 
#'
#' @param note_fn File name(s) for the text files containing the clinical notes. Can be 
#' a character string for an individual note, or a vector or list of file names for 
#' multiple notes.
#' @param drugnames Vector of drug names for which dosing information should be extracted. 
#' Can include various forms (e.g., generic, brand name) as well as abbreviations.
#' @param drgunit Unit of the drug being extracted, e.g., 'mg'
#' @param windowlength Length of the search window (in characters) around the drug name in 
#' which to search for dosing entities
#' @param max_edit_dist Maximum edit distance allowed when attempting to extract \code{drugnames}. 
#' Allows for capturing misspelled drug name information. 
#' @param ... Additional arguments to \code{medExtractR}
#'
#' @return A data.frame with the extracted dosing information
#' @export
#'

extractMed <- function(note_fn, drugnames, drgunit,
                        windowlength, max_edit_dist = 0, ...) {
  if(!(class(note_fn) %in% c("character", "list"))) {
    stop("`notefn` must be of class 'character' or 'list'")
  }
  if(!requireNamespace("medExtractR", quietly = TRUE)) {
    stop("extractMed requires the medExtractR package, please install it.",
      call. = FALSE)
  }
  library(medExtractR, warn.conflicts = FALSE, quietly = TRUE)
  s2f <- options()$stringsAsFactors
  options(stringsAsFactors = FALSE)
  on.exit(options(stringsAsFactors = s2f))
  addl <- list(...)
  addlvar <- names(addl)
  batchsize <- 1000
  if('batchsize' %in% addlvar) {
    batchsize <- addl[['batchsize']]
    addl[['batchsize']] <- NULL
  }
  progress <- TRUE
  if('progress' %in% addlvar) {
    progress <- addl[['progress']]
    addl[['progress']] <- NULL
  }
  doseArgs <- list(
    drug_names = drugnames,
    unit = drgunit,
    window_length = windowlength,
    max_dist = max_edit_dist
  )

  n <- length(note_fn)
  chunks <- ceiling(n / batchsize)
  dat <- vector('list', length = chunks)
  for(i in seq_along(dat)) {
    a <- (i - 1) * batchsize + 1
    b <- min(i * batchsize, n)
    if(progress) cat(sprintf("running batch %s of %s (%s%%)\r", a, n, round(100 * a / n)))
    dat[[i]] <- do.call(qrbind, lapply(note_fn[seq(a, b)], function(x) {
      do.call(getDose, c(x, doseArgs, addl))
    }))
  }
  if(progress) cat("\n")
  do.call(qrbind, dat)
}
