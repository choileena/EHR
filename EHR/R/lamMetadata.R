#' Example of Metadata for Lamotrigine Data
#'
#' An example of the metadata needed for the \code{\link{processLastDose}},
#' \code{\link{makeDose}}, and \code{\link{collapseDose}} functions.
#'
#' @format A data frame with 5 observations on the following variables.
#' \describe{
#'   \item{filename}{A character vector, filename for the clinical note}
#'   \item{pid}{A character vector, patient ID associated with the filename}
#'   \item{date}{A character vector, date associated with the filename}
#'   \item{note}{A character vector, note ID associated with the filename}
#' }
#'
#' @usage data(lam_metadata, package = 'EHR')
#'
#' @keywords datasets
#'
#' @examples
#' data(lam_metadata)
"lam_metadata"
