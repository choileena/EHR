#' Extract medication information from clinical notes
#'
#' This function is an interface to the \code{\link[medExtractR]{medExtractR}} function
#' within the \pkg{medExtractR} package, and allows drug dosing information to be extracted
#' from free-text sources, e.g., clinical notes.
#'
#' Medication information, including dosing data, is often stored in free-text sources such as
#' clinical notes. The \code{extractMed} function serves as a convenient wrapper for the
#' \pkg{medExtractR} package, a natural language processing system written in R for extracting
#' medication data. Within \code{extractMed}, the \code{\link[medExtractR]{medExtractR}} function
#' identifies dosing data for drug(s) of interest, specified by the \code{drugnames} argument,
#' using rule-based and dictionary-based approaches. Relevant dosing entities include medication
#' strength (identified using the \code{unit} argument), dose amount, dose given intake, intake
#' time or frequency of dose, dose change keywords (e.g., 'increase' or 'decrease'), and time of
#' last dose. For more details, see Weeks, et al. 2020. After applying \code{\link[medExtractR]{medExtractR}}
#' to extract drug dosing information, \code{extractMed} appends the file name to results to ensure
#' they are appropriately labeled.
#'
#' @param note_fn File name(s) for the text file(s) containing the clinical notes. Can be
#' a character string for an individual note, or a vector or list of file names for
#' multiple notes.
#' @param drugnames Vector of drug names for which dosing information should be extracted.
#' Can include various forms (e.g., generic, brand name) as well as abbreviations.
#' @param drgunit Unit of the drug being extracted, e.g., 'mg'
#' @param windowlength Length of the search window (in characters) around the drug name in
#' which to search for dosing entities
#' @param max_edit_dist Maximum edit distance allowed when attempting to extract \code{drugnames}.
#' Allows for capturing misspelled drug name information.
#' @param ... Additional arguments to \code{\link[medExtractR]{medExtractR}}, for example
#' \code{lastdose=TRUE} to extract time of last dose (see \pkg{medExtractR} package documentation for details)
#'
#' @return A data.frame with the extracted dosing information, labeled with file name as an identifier \cr
#' Sample output:\cr
#' \tabular{rrrr}{
#' filename \tab entity    \tab  expr   \tab    pos\cr
#' note_file1.txt \tab DoseChange\tab  decrease \tab  66:74\cr
#' note_file1.txt \tab DrugName   \tab Prograf \tab   78:85\cr
#' note_file1.txt \tab Strength  \tab  2 mg   \tab    86:90\cr
#' note_file1.txt \tab DoseAmt   \tab  1     \tab     91:92\cr
#' note_file1.txt \tab Frequency \tab  bid    \tab    101:104\cr
#' note_file1.txt \tab LastDose  \tab  2100    \tab   121:125\cr
#' }
#'
#' @examples
#' \donttest{
#' tac_fn <- list(system.file("examples", "tacpid1_2008-06-26_note1_1.txt", package = "EHR"),
#'                system.file("examples", "tacpid1_2008-06-26_note2_1.txt", package = "EHR"),
#'                system.file("examples", "tacpid1_2008-12-16_note3_1.txt", package = "EHR"))
#'
#' extractMed(tac_fn,
#'            drugnames = c("tacrolimus", "prograf", "tac", "tacro", "fk", "fk506"),
#'            drgunit = "mg",
#'            windowlength = 60,
#'            max_edit_dist = 2,
#'            lastdose=TRUE)
#' }
#' @export

extractMed <- function(note_fn, drugnames, drgunit,
                        windowlength, max_edit_dist = 0, ...) {
  if(!(class(note_fn) %in% c("character", "list"))) {
    stop("`notefn` must be of class 'character' or 'list'")
  }
  if(!requireNamespace("medExtractR", quietly = TRUE)) {
    stop("extractMed requires the medExtractR package, please install it.",
      call. = FALSE)
  }
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
  # data in EHR package must be manually loaded
  e <- new.env()
  if(!('dosechange_dict' %in% addlvar)) {
    utils::data('dosechange_vals', package = "medExtractR", envir = e)
    addl[['dosechange_dict']] <- e$dosechange_vals
  }
  if(!('drug_list' %in% addlvar)) {
    utils::data('rxnorm_druglist', package = "medExtractR", envir = e)
    addl[['drug_list']] <- e$rxnorm_druglist
  }
  if(!('freq_dict' %in% addlvar)) {
    utils::data('freq_vals', package = "medExtractR", envir = e)
    addl[['freq_dict']] <- e$freq_vals
  }
  if(!('intaketime_dict' %in% addlvar)) {
    utils::data('intaketime_vals', package = "medExtractR", envir = e)
    addl[['intaketime_dict']] <- e$intaketime_vals
  }
  rm(e)

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
    if(progress) cat(sprintf("running notes %s-%s in batch %s of %s (%s%%)\r", a, b, i, chunks, round(100 * i / chunks)))
    dat[[i]] <- do.call(qrbind, lapply(note_fn[seq(a, b)], function(x) {
      do.call(getDose, c(x, doseArgs, addl))
    }))
  }
  if(progress) cat("\n")
  do.call(qrbind, dat)
}
