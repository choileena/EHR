#' Run Demographic Data
#'
#' This module will load and modify demographic data.
#'
#' @param demo.path filename of demographic file (CSV, RData, RDS) or data.frame
#' @param demo.columns a named list that should specify columns in demo data; \sQuote{id},
#' is required.
#' @param toexclude expression that should evaluate to a logical, indicating if
#' the observation should be excluded
#' @param demo.mod.list list of expressions, giving modifications to make
#'
#' @details See EHR Vignette for Structured Data.
#'
#' @return list with two components
#'   \item{demo}{demographic data}
#'   \item{exclude}{vector of excluded visit IDs}
#'
#' @examples 
#' set.seed(2525)
#' dateSeq <- seq(as.Date('2019/01/01'), as.Date('2020/01/01'), by="day")
#' demo <- data.frame(mod_id_visit = 1:10,
#'                    weight.lbs = rnorm(10,160,20),
#'                    age = rnorm(10, 50, 10),
#'                    enroll.date = sample(dateSeq, 10))
#' tmpfile <- paste0(tempfile(), '.rds')
#' saveRDS(demo, file = tmpfile)
#'
#' # exclusion functions
#' exclude_wt <- function(x) x < 150
#' exclude_age <- function(x) x > 60
#' ind.risk <- function(wt, age) wt>170 & age>55
#' exclude_enroll <- function(x) x < as.Date('2019/04/01')
#'
#' # make demographic data that:
#' # (1) excludes ids with weight.lbs < 150, age > 60, or enroll.date before 2019/04/01
#' # (2) creates new 'highrisk' variable for subjects with weight.lbs>170 and age>55
#' out <- run_Demo(demo.path = tmpfile, demo.columns = list(id = 'mod_id_visit'),
#'                toexclude = expression(
#'                  exclude_wt(weight.lbs)|exclude_age(age)|exclude_enroll(enroll.date)
#'                ),
#'                demo.mod.list = list(highrisk = expression(ind.risk(weight.lbs, age))))
#' 
#' out
#'
#' @export

run_Demo <- function(demo.path, demo.columns = list(), toexclude, demo.mod.list) {
  # read and transform data
  demo.in <- read(demo.path)
  demo <- dataTransformation(demo.in, modify = demo.mod.list)
  demo.req <- list(id = NA)
  demo.col <- validateColumns(demo, demo.columns, demo.req)

  # exclusion criteria
  if (missing(toexclude)) {
    parsed.excl <- logical(nrow(demo))
  } else {
    parsed.excl <- eval(toexclude, demo)
  }

  excl.id <- demo[parsed.excl, demo.col$id] # the list of subject_id that should be excluded
  message(sprintf('The number of subjects in the demographic data, who meet the exclusion criteria: %s', length(excl.id)))

  list(demo = demo, exclude = excl.id)
}

#' Merge in Demographic Data
#'
#' This module will merge demographic data with a base data set by id.
#'
#' @param dat Base data set, a filename (CSV, RData, RDS), or data.frame
#' @param dat.columns a named list that should specify columns in data.
#' \sQuote{id} is required. \sQuote{idvisit} may also be specified;
#' \sQuote{idvisit} can be used when there are multiple visits 
#' (i.e., several occasions) for the same subject.
#' @param demo.list demographic information, if available; the output from 
#' \code{\link{run_Demo}} or a correctly formatted data.frame
#' @param demo.columns a named list that should specify columns in demographic data;
#' \sQuote{id} is required. \sQuote{idvisit} may also be used to specify
#' columns for the unique idvisit. Any other columns
#' present in the demographic data are treated as covariates.
#'
#' @return data.frame
#'
#' @examples
#' user <- data.frame(id = c('A','A','B'), event = c('A.1','A.2','B.1'), enroll = '2025-02-25')
#' demo <- data.frame(id = c('A', 'B'), age = c(25, 45))
#' demo_w_visit <- data.frame(id = c('A','A','B'), event = c('A.1','A.2','B.1'), age = c(25, 26, 45))
#' add_Demo(user, list(id = 'id'), demo, list(id = 'id'))
#' add_Demo(user, list(id = 'id', idvisit = 'event'), demo_w_visit, list(id = 'id', idvisit = 'event'))
#' @export

add_Demo <- function(dat, dat.columns, demo.list, demo.columns) {
  dat.req <- list(id = NA, idvisit = NULL)
  demo.req <- list(id = NA, idvisit = NULL)

  dat.col <- validateColumns(dat, dat.columns, dat.req)
  hasMIV <- 'idvisit' %in% names(dat.col)

  demoData <- NULL
  if(inherits(demo.list, 'data.frame')) {
    demoData <- demo.list
  } else {
    if('demo' %in% names(demo.list)) {
      demoData <- demo.list$demo
    }
  }
  if(is.null(demoData)) {
    stop('Demographic data was provided in an unexpected format.')
  }
  demo.col <- validateColumns(demoData, demo.columns, demo.req)

  if(hasMIV & !('idvisit' %in% names(demo.col))) {
    stop('Demographic data missing idvisit variable though specified for dat')
  }
  merge(dat, demoData, by.x=c(dat.col$idvisit, dat.col$id), by.y=c(demo.col$idvisit, demo.col$id), all.x=TRUE)
}
