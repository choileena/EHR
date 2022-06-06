#' Internal functions for Extract-Med module
#'
#' These internal functions aid the main function used in the Extract-Med module (\code{\link{extractMed}}).
#' 
#' \code{getNote}: Takes the file name and reads in the note as a character vector
#' 
#' \code{getDose}: Runs \code{\link[medExtractR]{medExtractR}} on a single clinical note (file) and labels 
#' the results output with file name
#'
#' @name extractMed-internal
#' @aliases getNote getDose
#' @keywords internal

# Reads in note given a file name
getNote <- function(note) {
  paste(scan(note, '', sep = '\n', quiet = TRUE), collapse = '\n')
}

# Runs medExtractR, formats output to include filename
getDose <- function(note, ...) {
  d <- medExtractR::medExtractR(getNote(note), ...)
  # return NULL if d is NA
  bad <- length(d) == 1 && all(is.na(d))
  if(bad) return(NULL)

  # Use file name to label the extractions
  d[,'filename'] <- basename(note)

  d <- d[, c('filename', 'entity', 'expr', 'pos')]

  return(d)
}
