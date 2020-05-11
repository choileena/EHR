#' Example of Lab Time Data for Tacrolimus
#'
#' An example dataset used in \code{\link{processLastDose}} that contains lab time data. This dataset should
#' have one row per patient ID-date pair, and contain the time a lab was performed as a datetime variable.
#'
#' @format A data frame with 2 observations on the following variables.
#' \describe{
#'   \item{pid}{A character vector, patient ID associated with the lab value}
#'   \item{date}{A character vector, date associated with the lab value}
#'   \item{labtime}{A POSIXct vector, datetime at which the lab was performed formatted as YYYY-MM-DD HH:MM:SS}
#' }
#'
#' @usage data(tac_lab, package = 'EHR')
#'
#' @keywords datasets
#'
#' @examples
#' data(tac_lab)
"tac_lab"
