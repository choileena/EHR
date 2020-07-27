#' Run Demographic Data
#'
#' This module will load and modify demographic data.
#'
#' @param demo.path filename of a lab file (stored as RDS)
#' @param toexclude expression that should evaluate to a logical, indicating if
#' the observation should be excluded
#' @param demo.mod.list list of expressions, giving modifications to make
#'
#' @return list with two components
#'   \item{demo}{demographic data}
#'   \item{exclude}{vector of excluded visit IDs}
#'
#' @export

run_Demo <- function(demo.path, toexclude, demo.mod.list) {
  # read and transform data
  demo.in <- readRDS(demo.path)
  demo <- dataTransformation(demo.in, modify = demo.mod.list)

  # exclusion criteria
  if (missing(toexclude)) {
    parsed.excl <- logical(nrow(demo))
  } else {
    parsed.excl <- eval(toexclude, demo)
  }

  excl.id <- demo[parsed.excl, 'mod_id_visit'] # the list of subject_id that should be excluded
  cat(sprintf('The number of subjects in the demographic data, who meet the exclusion criteria: %s\n', length(excl.id)))

  list(demo = demo, exclude = excl.id)
}
