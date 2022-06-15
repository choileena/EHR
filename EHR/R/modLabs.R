#' Run Lab Data
#'
#' This module will load and modify laboratory data.
#'
#' @param lab.path filename of a lab file (CSV, RData, RDS), or data.frame
#' @param lab.select columns to select
#' @param lab.mod.list list of expressions giving modifications to make;
#'  passed to \code{\link{dataTransformation}}
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return lab data set
#'
#' @examples 
#' lab_data <- data.frame(mod_id=rep(1:3,each=3),
#'                        date=rep(c("01/12/17","05/05/18","11/28/16"),each=3),
#'                        time=rep(c("1:30","2:30","3:30"),3),
#'                        creat=rnorm(9,0.5,0.05))
#' run_Labs(lab_data, lab.mod.list=list(log_creat=expression(log(creat))))
#'
#' @export

run_Labs <- function(lab.path, lab.select, lab.mod.list) {
  lab.in <- read(lab.path)
  lab <- dataTransformation(lab.in, modify = lab.mod.list)
  lab[,lab.select]
}
