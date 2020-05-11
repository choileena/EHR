#' Internal functions for parse process
#'
#' These internal functions aid the main functions used in the parsing process 
#' (\code{\link{parseMedExtractR}}, \code{\link{parseMedXN}},
#' \code{\link{parseMedEx}}, \code{\link{parseCLAMP}}). 
#' 
#' \code{medxnColonFormat}: converts entity information into the 
#' form "extracted expression::start position::stop position", similar 
#' to how output is formatted by default in MedXN output.  
#' 
#' \code{entorder}: a helper function that orders the entities by start position  
#' 
#' \code{medxnEntityFormat}: a helper function that applies both \code{medxnColonFormat} 
#' to convert entities into the "extraction::start::stop" format and \code{entorder} to 
#' sort them. It then collapses the entities and separates the extractions with backticks
#'
#' @name parse-internal
#' @aliases medxnColonFormat entorder medxnEntityFormat
#' @keywords internal
NULL

# MedXN format is VALUE::START_POSITION::END_POSITION
medxnColonFormat <- function(x) {
  sub('^(.*):([0-9]*):([0-9]*)$', '\\1::\\2::\\3', x)
}

entorder <- function(x) {
  x[order(as.numeric(sub('.*:', '', x)))]
}

medxnEntityFormat <- function(x) {
  paste(medxnColonFormat(entorder(x)), collapse = '`')
}
