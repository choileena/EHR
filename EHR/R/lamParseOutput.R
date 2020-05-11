#' Example of Lamotrigine Output from `parseMedExtractR`
#'
#' The output after running \code{\link{parseMedExtractR}} on 4 example clinical notes.
#'
#' @format A data frame with 10 observations on the following variables.
#' \describe{
#'   \item{filename}{A character vector, filename for the clinical note}
#'   \item{drugname}{A character vector, drug name extracted from the clinical note along with start and stop positions}
#'   \item{strength}{A character vector, strengths extracted from the clinical note along with start and stop positions}
#'   \item{dose}{A character vector, dose amounts extracted from the clinical note along with start and stop positions}
#'   \item{route}{A character vector, routes extracted from the clinical note along with start and stop positions}
#'   \item{freq}{A character vector, frequencies extracted from the clinical note along with start and stop positions}
#'   \item{dosestr}{A character vector, dose intakes extracted from the clinical note along with start and stop positions}
#'   \item{dosechange}{A character vector, dose change keywords extracted from the clinical note along with start and stop positions}
#'   \item{lastdose}{A character vector, last dose times extracted from the clinical note along with start and stop positions}
#' }
#'
#' @usage data(lam_mxr_parsed, package = 'EHR')
#'
#' @keywords datasets
#'
#' @examples
#' data(lam_mxr_parsed)
"lam_mxr_parsed"
