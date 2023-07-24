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

#' Merge in Lab Data
#'
#' This module will merge laboratory data with a base data set by closest date.
#'
#' @param dat Base data set, a filename (CSV, RData, RDS), or data.frame
#' @param dat.columns a named list that should specify columns in data.
#' \sQuote{id} and \sQuote{datetime} are required. \sQuote{datetime} can refer
#' to a single date-time variable (datetime = \sQuote{date_time}) or two
#' variables holding date and time separately (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})).
#' @param lab.list lab data, if available; the output from \code{\link{run_Labs}} or 
#' a correctly formatted list
#' @param lab.columns a named list that should specify columns in lab data; \sQuote{id},
#' and \sQuote{datetime} are required. \sQuote{datetime} is the date and time when
#' the lab data was obtained, which can refer to a single date-time variable
#' (datetime = \sQuote{date_time}) or two variables holding date and time separately
#' (e.g., datetime = c(\sQuote{Date}, \sQuote{Time})). Any other columns present in lab
#' data are treated as lab values.
#' @param labPriorWindow This value sets the time frame window with the number of days
#' prior to the first date.time event in dat; defaults to 7.
#'
#' @return data.frame
#'
#' @examples
#' user <- data.frame(id = c('A', 'B'), datetime = '2025-02-25 10:00')
#' labs <- structure(list(
#'     id = c("A", "A", "A", "B", "B", "B", "B", "B", "B", "B"),
#'     dt = structure(c(
#'       1738800000, 1756684800, 1775865600, 1726790400, 1737763200,
#'       1741910400, 1742169600, 1750118400, 1779062400, 1782345600
#'       ), class = c("POSIXct", "POSIXt"), tzone = ""),
#'     weight = c(155L, 163L, 169L, 170L, 172L, 175L, 183L, 187L, 197L, 198L),
#'     a1c = c(5.93, 5.97, 5.98, 5.99, 6.04, 6.04, 6.05, 6.08, 6.16, 6.17)
#'   ), class = "data.frame", row.names = c(NA, -10L)
#' )
#' add_Labs(user, list(id = 'id', datetime = 'datetime'), labs, list(id = 'id', datetime = 'dt'), labPriorWindow = 30)
#' @export

add_Labs <- function(dat, dat.columns, lab.list, lab.columns, labPriorWindow = 7) {
  dat.req <- list(id = NA, datetime = NA)
  lab.req <- list(id = NA, datetime = NA)

  dat.col <- validateColumns(dat, dat.columns, dat.req)
  keepDT <- FALSE
  if(length(dat.col$datetime) == 2) {
    datDT <- paste(dat[,dat.col$datetime[1]], dat[,dat.col$datetime[2]])
  } else {
    datDT <- dat[,dat.col$datetime]
    # forcing new D-T variable; keep it if already present
    keepDT <- dat.col$datetime == 'date.time'
  }
  dat[,'date.time'] <- pkdata::parse_dates(datDT)

  lab.vars <- c()
  # if input is single DF, turn into list
  if(inherits(lab.list, 'data.frame')) {
    lab.list <- list(lab.list)
  }
  for(i in seq_along(lab.list)) {
    lab.col <- validateColumns(lab.list[[i]], lab.columns, lab.req)
    if(length(lab.col$datetime) == 2) {
      labdt <- paste(lab.list[[i]][,lab.col$datetime[1]], lab.list[[i]][,lab.col$datetime[2]])
    } else {
      labdt <- lab.list[[i]][,lab.col$datetime]
    }
    lab.list[[i]][,'date.time'] <- pkdata::parse_dates(labdt)
    cln <- names(lab.list[[i]])
    cln <- setdiff(cln, c(lab.col$id, 'date.time', lab.col$datetime))
    lab.vars <- c(lab.vars, cln)
    if(length(cln)) {
      curlab <- lab.list[[i]][,c(lab.col$id, 'date.time', cln)]
      labMax <- labPriorWindow * 24
      dat <- merge_by_time(dat, curlab, maxTime=labMax, x.id=dat.col$id, y.id=lab.col$id, x.time='date.time', y.time='date.time')
    }
  }
  missLab <- setdiff(lab.vars, names(dat))
  if(length(missLab)) {
    stop(sprintf('there was a problem merging lab variables: %s', paste(missLab, collapse = ', ')))
  }
  if(!keepDT) dat[,'date.time'] <- NULL
  dat
}
