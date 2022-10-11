#' PK oral functions
#'
#' code{build_lastdose}: build PK oral with lastdose information
#'
#' @keywords internal

build_lastdose <- function(x, first_interval_hours = 336, ldCol = NULL) {
  nr <- nrow(x)
  names(x)[1:3] <- c('id','dt','dose')
  cnames <- names(x)
  # save covariates for later
  covars <- x[,setdiff(cnames, c('id','dt','dose',ldCol))]
  dt_index <- format(x[,'dt'] + 30 * 60, '%Y-%m-%d %H:%M:%S')

  # last dose variable
  if(!is.null(ldCol)) {
    stopifnot(ldCol %in% cnames)
    lastdosetime <- format(x[,ldCol], '%Y-%m-%d %H:%M:%S')
  } else {
    lastdosetime <- rep(NA_character_, nr)
  }
  hasLD <- !is.na(lastdosetime)
  ld_row <- which(hasLD)

  first_interval <- first_interval_hours * 3600
  firstTime <- x[1,'dt'] - first_interval
  t1 <- c(firstTime, x[-nr,'dt'])
  t2 <- x[,'dt']
  t2[hasLD] <- lastdosetime[hasLD]
  t2 <- t2 - 3600 * 6
  # count 12-hour intervals between t1 and t2
  addl_vals <- c(as.numeric(difftime(t2, t1, units = 'hours')) %/% 12, NA)

  dat <- cbind(data.frame(CID=x[1,'id'], time = NA_integer_, date=dt_index, conc=NA, dose=x[, 'dose'], addl=addl_vals[-1], II=12, mdv=1), covars)
  dat <- rbind(dat[1,], dat)
  dat[1,'date'] <- format(firstTime, '%Y-%m-%d %H:%M:%S')
  dat[1,'addl'] <- addl_vals[1]
  # update II if addl is zero
  dat[!is.na(dat[,'addl']) & dat[,'addl'] == 0, 'II'] <- NA
  if(length(ld_row)) {
    wld <- dat[ld_row+1,]
    wld[,'date'] <- lastdosetime[ld_row]
    wld[,'addl'] <- 0
    wld[,'II'] <- NA
    # dose should come from previous row
    wld[,'dose'] <- dat[ld_row,'dose']
    # always remove the last row of dat
    newdat <- rbind(dat[-nrow(dat),], wld)
    newdat <- newdat[order(newdat[,'date']),]
  } else {
    newdat <- dat[-nrow(dat),]
  }
  newdat
}

#' Build-PK-Oral Module
#'
#' This module builds PK data for orally administered medications.
#'
#' @param x a data.frame or file saved as either CSV, RData, or RDS
#' @param idCol data.frame id column name
#' @param dtCol data.frame date column name
#' @param doseCol dose column name
#' @param concCol concentration column name
#' @param ldCol last-dose time column name
#' @param first_interval_hours number of hours before the first concentration to start time=0; the default is 336 hours = 14 days
#' @param imputeClosest columns to impute missing data with next observation propagated backward; this is in addition to
#' all covariates receving imputation using last observation carried forward
#'
#' @details See EHR Vignette for Build-PK-Oral.
#'
#' @return data.frame
#'
#' @examples
#' ## Data Generating Function
#' mkdat <- function() {
#'   npat <- 3
#'   visits <- floor(runif(npat, min=2, max=6))
#'   id <- rep(1:npat, visits)
#'   dt_samp <- as.Date(sort(sample(700, sum(visits))), origin = '2019-01-01')
#'   tm_samp <- as.POSIXct(paste(dt_samp, '10:00:00'), tz = 'UTC')
#'   dt <- tm_samp + rnorm(sum(visits), 0, 1*60*60)
#'   dose_morn <- sample(c(2.5,5,7.5,10), sum(visits), replace = TRUE)
#'   conc <- round(rnorm(sum(visits), 1.5*dose_morn, 1),1)
#'   ld <- dt - sample(10:16, sum(visits), replace = TRUE) * 3600
#'   ld[rnorm(sum(visits)) < .3] <- NA
#'   age <- rep(sample(40:75, npat), visits)
#'   gender <- rep(sample(0:1, npat, replace=TRUE), visits)
#'   weight <- rep(round(rnorm(npat, 180, 20)),visits)
#'   hgb <- rep(rnorm(npat, 10, 2), visits)
#'   data.frame(id, dt, dose_morn, conc, ld, age, gender, weight, hgb)
#' }
#'
#' # Make raw data
#' set.seed(30)
#' dat <- mkdat()
#'
#' #Process data without last-dose times
#' run_Build_PK_Oral(x = dat,
#'                   idCol = "id",
#'                   dtCol = "dt",
#'                   doseCol = "dose_morn",
#'                   concCol = "conc",
#'                   ldCol = NULL,
#'                   first_interval_hours = 336,
#'                   imputeClosest = NULL)
#' 
#' #Process data with last-dose times
#' run_Build_PK_Oral(x = dat, doseCol = "dose_morn", ldCol = "ld")
#'
#' @export

run_Build_PK_Oral <- function(x, idCol = 'id', dtCol = 'dt', doseCol = 'dose', concCol = 'conc', ldCol = NULL,
                              first_interval_hours = 336, imputeClosest = NULL) {
  tz <- Sys.timezone()

  x <- read(x)
  x_cols <- names(x)
  exp_cols <- c(idCol, dtCol, doseCol, concCol)
  stopifnot(all(exp_cols %in% x_cols))
  # save covariates for later
  covars <- setdiff(x_cols, c(exp_cols, ldCol))

  dt_str <- format(x[,dtCol], '%Y-%m-%d %H:%M:%S')
  # convert time to UTC
  x[,dtCol] <- as.POSIXct(dt_str, tz = tz)

  # pk_form for conc
  conc.dat <- cbind(data.frame(CID = x[,idCol], time = NA_integer_, date = dt_str, conc = x[,concCol], dose = NA, addl = NA, II = NA, mdv = 0), x[,covars])

  dose <- x[,c(idCol, dtCol, doseCol, ldCol, covars)]
  sl <- split(dose, dose[,idCol])
  ll <- lapply(sl, build_lastdose, ldCol = ldCol, first_interval_hours = first_interval_hours)
  dose.dat <- do.call(qrbind, ll)

  xx <- rbind(conc.dat, dose.dat)
  # convert from string to date-time
  xx[,'date'] <- as.POSIXct(xx[,'date'], tz = tz)
  xx <- xx[order(xx[,'CID'], xx[,'date']),]
  rownames(xx) <- NULL

  # LOCF for covariates
  # only borrow if previous row has same CID
  canBorrow <- c(FALSE, xx[-nrow(xx),'CID'] == xx[-1,'CID'])
  for(i in covars) {
    miss_ix <- which(is.na(xx[,i]) & canBorrow)
    for(j in miss_ix) {
      xx[j,i] <- xx[j-1,i]
    }
  }
  # NOCB
  canBorrow <- c(canBorrow[-1], FALSE)
  for(i in intersect(x_cols, imputeClosest)) {
    # work in reverse order
    miss_ix <- rev(which(is.na(xx[,i]) & canBorrow))
    for(j in miss_ix) {
      xx[j,i] <- xx[j+1,i]
    }
  }

  # create 'time' as hours from first timepoint
  t0 <- as.POSIXct(tapply(xx[,'date'], xx[,'CID'], min), origin = '1970-01-01', tz = tz)
  xx_t0 <- t0[match(xx[,'CID'], names(t0))]
  xx[,'time'] <- round(as.numeric(difftime(xx[,'date'], xx_t0, units = 'hours')), 2)
  xx[,'date'] <- format(xx[,'date'], '%Y-%m-%d %H:%M:%S')
  # remove rows with negative `addl`
  xx <- xx[is.na(xx[,'addl']) | xx[,'addl'] >= 0,]
  names(xx)[match(c('conc','dose'), names(xx))] <- c('dv','amt')
  xx[,'evid'] <- +(!is.na(xx[,'amt']))
  # set column order
  reqOrder <- c('CID', 'time', 'amt', 'dv', 'mdv', 'evid', 'addl', 'II')
  pkOrder <- c(reqOrder, setdiff(names(xx), reqOrder))
  xx <- xx[,pkOrder]
  # restore idCol name
  names(xx)[1] <- idCol
  xx
}
