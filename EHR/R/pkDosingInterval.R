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
  dt_index <- as.character(x[,'dt'] + 30 * 60, '%Y-%m-%d %H:%M:%S')

  # last dose variable
  if(!is.null(ldCol)) {
    stopifnot(ldCol %in% cnames)
    lastdosetime <- as.character(x[,ldCol], '%Y-%m-%d %H:%M:%S')
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
  dat[1,'date'] <- as.character(firstTime, '%Y-%m-%d %H:%M:%S')
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

#' PK oral data
#'
#' This module builds oral data for PK analysis.
#'
#' @param x data.frame
#' @param idCol data.frame id column name
#' @param dtCol data.frame date column name
#' @param doseCol dose column name
#' @param concCol concentration column name
#' @param ldCol last-dose time column name
#' @param first_interval_hours number of hours before first imputed time point
#' @param imputeClosest columns to impute with closest value
#'
#' @return data.frame
#'
#' @export

run_Build_PK_Oral <- function(x, idCol = 'id', dtCol = 'dt', doseCol = 'dose', concCol = 'conc', ldCol = NULL,
                              first_interval_hours = 336, imputeClosest = NULL) {
  tz <- Sys.timezone()

  x_cols <- names(x)
  exp_cols <- c(idCol, dtCol, doseCol, concCol)
  stopifnot(all(exp_cols %in% x_cols))
  # save covariates for later
  covars <- setdiff(x_cols, c(exp_cols, ldCol))

  dt_str <- as.character(x[,dtCol], '%Y-%m-%d %H:%M:%S')
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
  xx[,'date'] <- as.character(xx[,'date'], '%Y-%m-%d %H:%M:%S')
  # remove rows with negative `addl`
  xx[is.na(xx[,'addl']) | xx[,'addl'] >= 0,]
}
