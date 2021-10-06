#' Internal functions for EHR modules
#' 
#' code{colflow_mod}: build a comparison of potentially duplicate flow data
#'
#' code{findMedFlowRow_mod}: find row numbers for matching ID and time
#'
#' code{flowData_mod}: correct flow data
#'
#' code{concData_mod}: correct concentration data
#'
#' code{infusionData_mod}: combine flow and MAR data; standardize hourly rate,
#' which may or may not include weight
#'
#' code{resolveDoseDups_mod}: correct duplicated infusion and bolus doses
#'
#' code{processErx}: prepare e-prescription data
#'
#' code{processErxAddl}: additional processing, checking for connected observations
#'
#' code{validateColumns}: check columns are in place
#'
#' code{read}: generic read function
#'
#' @name mod-internal
#' @aliases colflow_mod findMedFlowRow_mod flowData_mod concData_mod
#' infusionData_mod resolveDoseDups_mod processErx processErxAddl validateColumns read
#' @keywords internal
NULL

colflow_mod <- function(dat) {
  sd <- split(dat, dat[,'date.time'])
  sn <- length(sd)
  mc <- c('mod_id','date.time','mod_id_visit','weight','rate','final.units')
  res <- dat[rep(1, sn), mc]
  mc2 <- c('i2', 'w2', 'r2', 'u2')
  res[, mc2] <- NA
  for(i in seq(sn)) {
    ssd <- sd[[i]]
    nr <- nrow(ssd)
    if(nr == 1) {
      res[i,mc] <- ssd[1, mc]
    } else if(nr == 2) {
      res[i,mc] <- ssd[1, mc]
      res[i,mc2] <- ssd[2, mc[3:6]]
    } else {
      res[i,mc[3:6]] <- NA
    }
  }
  res
}

findMedFlowRow_mod <- function(df, id, dt) {
    rnums <- numeric(length(id))
    for(i in seq_along(rnums)) {
        rnums[i] <- which(df[,'mod_id_visit'] == id[i] & df[,'date.time'] == dt[i])
    }
    rnums
}

flowData_mod <- function(dat, checkDir, failflow_filename, giveExample = TRUE) {
  # dat should be standardized by this point
  # reorder by id
  dat <- dat[order(dat[,'mod_id_visit']),]
  # impute missing weight
  ix <- which(is.na(dat[,'weight']))
  for(i in seq_along(ix)) {
    row <- dat[ix[i], c('mod_id_visit','date.time')]
    opt <- dat[dat[,'mod_id_visit'] == row[[1]], c('date.time','weight')]
    dat[ix[i],'weight'] <- takeClosest(row[[2]], opt[[1]], opt[[2]])
  }
  otherCols <- setdiff(names(dat), 'mod_id_visit')
  # remove duplicates
  n1 <- nrow(dat)
  dat <- dat[!duplicated(dat[,otherCols]),]
  n2 <- nrow(dat)
  cat(sprintf('The number of rows in the original data           %8d
The number of rows after removing the duplicates  %8d\n', n1, n2))
  
  key <- paste(dat[,'mod_id'], dat[,'date.time'], sep = '|')
  repflow <- unique(key[duplicated(key)])
  posdup <- unique(dat[key %in% repflow, 'mod_id'])
  dd <- dat[dat[,'mod_id'] %in% posdup,]
  dd <- dd[order(dd[,'mod_id'], dd[,'date.time']),]

  if(nrow(dd)) { # hotfix for vignette data - only process if dd has >0 rows  
    ldd <- split(dd, dd[,'mod_id'])
    ddd <- do.call(rbind, lapply(ldd, colflow_mod))
    pddd <- ddd[!is.na(ddd[,'i2']),]
    did <- pddd[,'mod_id_visit'] == pddd[,'i2']
    pd1 <- pddd[did,] # mod_id_visit is duplicated with inconsistent rows at the same date.time
    pd2 <- pddd[!did,] # mod_id_visit is not duplicated with inconsistent rows at the same date.time
    badRows <- vector('list', 5)
    # specify key for duplicates, mod_id|date.time
    # specify cols to check for duplicates (in colflow), mod_id_visit|weight|rate|final.units

    # rate of 0, needs file correction
    fn <- file.path(checkDir, paste0('fail', failflow_filename, '.csv'))
    fixfn <- sub('fail', 'fix', fn)
    ix <- which(pddd[,'rate'] == 0 | pddd[,'r2'] == 0)
    cat(sprintf('%s invalid duplicate rows with rate = 0 (requires correction by subject)\n', length(ix)))
    if(length(ix)) {
      if(giveExample) {
        print(head(pddd[ix,],3), row.names = FALSE)
      }
      id <- sort(unique(c(pddd[ix,'mod_id_visit'], pddd[ix,'i2'])))
      rnums <- vector('list', length(id))
      r1 <- vector('list', length(id))
      r2 <- vector('list', length(id))
      for(i in seq_along(id)) {
        rnums[[i]] <- which(dat[,'mod_id_visit'] == id[i])
        r1[[i]] <- which(pd1[,'mod_id_visit'] == id[i])
        r2[[i]] <- which(pd2[,'mod_id_visit'] == id[i])
      }
      rnums <- sort(unlist(rnums))
      r1 <- sort(unlist(r1))
      r2 <- sort(unlist(r2))
      if(length(rnums)) {
        needfix <- dat[rnums,]
        dat <- dat[-rnums,]
        if(!is.null(checkDir)) {
          msg <- sprintf('%s rows need review, see file %s AND create %s\n', length(rnums), fn, fixfn)
          writeCheckData(needfix, fn, msg)
          if(file.access(fixfn, 4) != -1) {
            hasfix <- read.csv(fixfn, stringsAsFactors = FALSE)
            if(nrow(hasfix)) {
              dat <- rbind(dat, hasfix[,names(dat)])
              cat(sprintf('file %s read with failures replaced\n', fixfn))
            }
          }
        }
      } else {
        cat(sprintf('no failures, file %s not created\n', fn))
      }
      if(length(r1)) pd1 <- pd1[-r1,]
      if(length(r2)) pd2 <- pd2[-r2,]
    } else {
      cat(sprintf('no failures, file %s not created\n', fn))
    }

    # Subject.id is duplicated with conflicting rows for final.units, when the rate is non-zero and one (or both) of final.units = 0
    # Mark (remove) the rows where final.units = 0
    cat(sprintf('%s invalid duplicate rows with final.units = 0\n', nrow(pd1)))
    if(nrow(pd1) > 0) {
      if(giveExample) {
        #print(pd1[1:3,], row.names = FALSE)
        print(head(pd1,3), row.names = FALSE)
      }
      id <- pd1[,'mod_id_visit']
      dt <- pd1[,'date.time']
      rnums <- vector('list', length(id))
      for(i in seq_along(rnums)) {
        rnums[[i]] <- which(dat[,'mod_id_visit'] == id[i] & dat[,'date.time'] == dt[i] & dat[,'final.units'] == 0)
      }
      badRows[[1]] <- unlist(rnums)
    }

    # Subject.id is not duplicated and rate is missing
    # Mark (remove) all rows
    ix <- is.na(pd2[,'rate']) & is.na(pd2[,'r2'])
    cat(sprintf('%s invalid duplicate rows with rate missing\n', sum(ix)))
    if(any(ix)) {
      if(giveExample) {
        #print(pd2[ix,][1:3,], row.names = FALSE)
        print(head(pd2[ix,],3), row.names = FALSE)
      }
      id <- c(pd2[ix,'mod_id_visit'], pd2[ix,'i2'])
      dt <- c(pd2[ix,'date.time'], pd2[ix,'date.time'])
      badRows[[2]] <- findMedFlowRow_mod(dat, id, dt)
    }
    pd3 <- pd2[!ix,]

    # one rate or unit is missing
    ix1 <- (is.na(pd3[,'rate']) & !is.na(pd3[,'r2'])) | (is.na(pd3[,'final.units']) & !is.na(pd3[,'u2']))
    ix2 <- (!is.na(pd3[,'rate']) & is.na(pd3[,'r2'])) | (!is.na(pd3[,'final.units']) & is.na(pd3[,'u2']))
    ix <- ix1 | ix2
    cat(sprintf('%s invalid duplicate rows with one rate or unit missing\n', sum(ix)))
    if(any(ix)) {
      if(giveExample) {
        #print(pd3[ix,][1:3,], row.names = FALSE)
        print(head(pd3[ix,],3), row.names = FALSE)
      }
      rn1 <- findMedFlowRow_mod(dat, pd3[ix1,'mod_id_visit'], pd3[ix1,'date.time'])
      rn2 <- findMedFlowRow_mod(dat, pd3[ix2,'i2'], pd3[ix2,'date.time'])
      badRows[[3]] <- c(rn1, rn2)
    }
    pd4 <- pd3[!ix,]

    # Subject.id is not duplicated, but conflicting rows for final.units
    # when the rate is non-zero and one final.units = 0
    ix1 <- pd4[,'final.units'] != 0 & pd4[,'u2'] == 0
    ix2 <- pd4[,'final.units'] == 0 & pd4[,'u2'] != 0
    ix <- ix1 | ix2
    cat(sprintf('%s invalid duplicate rows with one unit = 0\n', sum(ix)))
    if(any(ix)) {
      if(giveExample) {
        #print(pd4[ix,][1:3,], row.names = FALSE)
        print(head(pd4[ix,],3), row.names = FALSE)
      }
      rn1 <- findMedFlowRow_mod(dat, pd4[ix1,'mod_id_visit'], pd4[ix1,'date.time'])
      rn2 <- findMedFlowRow_mod(dat, pd4[ix2,'i2'], pd4[ix2,'date.time'])
      badRows[[4]] <- c(rn1, rn2)
    }
    pd5 <- pd4[!ix,]

    # Duplicate, but discrepancy
    # Mark (remove) the rows that have no additional observations and keep the row
    # that has any records after the time point of conflict (by our rule)
    nr <- nrow(pd5)
    cat(sprintf('%s discrepant rows\n', nr))
    if(nr > 0) {
      if(giveExample) {
        #print(pd5[1:3,], row.names = FALSE)
        print(head(pd5,3), row.names = FALSE)
      }
      id1 <- pd5[,'mod_id_visit']
      id2 <- pd5[,'i2']
      dt <- pd5[,'date.time']
      rnums <- numeric(length(dt))
      for(i in seq_along(rnums)) {
        a1 <- sum(dat[,'mod_id_visit'] == id1[i] & dat[,'date.time'] > dt[i])
        a2 <- sum(dat[,'mod_id_visit'] == id2[i] & dat[,'date.time'] > dt[i])
        if(a1 == 0) {
          rnums[i] <- which(dat[,'mod_id_visit'] == id1[i] & dat[,'date.time'] == dt[i])
        } else if (a2 == 0) {
          rnums[i] <- which(dat[,'mod_id_visit'] == id2[i] & dat[,'date.time'] == dt[i])
        }
      }
      badRows[[5]] <- rnums
    }

    # remove all bad rows
    allBadRows <- sort(unlist(badRows))
    n1 <- nrow(dat)
    n2 <- length(allBadRows)
    dat <- dat[-allBadRows,]
    n3 <- nrow(dat)
    cat(sprintf('The number of rows before removing bad rows %8d
  The number of bad rows                      %8d
  The number of rows after removing bad rows  %8d\n', n1, n2, n3))
  }

  # reorder by id, date.time
  dat[order(dat[,'mod_id_visit'], dat[,'date.time']),]
}

concData_mod <- function(dat, sampFile, lowerLimit = NA, drugname = NULL, giveExample = TRUE,
                         checkDir = NULL, dem=NULL,
                         failmissconc_filename,
                         multsets_filename,
                         faildupconc_filename) {

  # dat should be standardized by this point
  hasSamp <- !is.null(sampFile)
  if(hasSamp) {
    concdate <- merge(dat[,c('mod_id_event'),drop=FALSE], sampFile[,c('mod_id_event','Sample.Collection.Date.and.Time')], by='mod_id_event', all.x = TRUE)
    names(concdate)[2] <- 'date.time'
  } else {
    concdate <- dat[,c('mod_id_event','date.time')]
  }
  dt <- pkdata::parse_dates(fixDates(concdate[,'date.time']))

  fn <- file.path(checkDir, paste0('fail', failmissconc_filename, drugname, '.csv'))
  fixfn <- sub('fail', 'fix', fn)
  rnums <- which(is.na(dt))
  if(is.null(checkDir)) {
    # bypassing check
  } else if(length(rnums)) {
    needfix <- concdate[rnums,]
    concdate <- concdate[-rnums,]
    msg <- sprintf('%s rows need review, see file %s AND create %s\n', length(rnums), fn, fixfn)
    writeCheckData(needfix, fn, msg)
    if(file.access(fixfn, 4) != -1) {
      hasfix <- read.csv(fixfn, stringsAsFactors = FALSE)
      if(nrow(hasfix)) {
        cn <- names(concdate)
        concdate <- rbind(concdate, hasfix[,cn])
        cat(sprintf('file %s read with failures replaced\n', fixfn))
      }
    }
  } else {
    cat(sprintf('no failures, file %s not created\n', fn))
  }
  if(hasSamp) {
    dt <- concdate[match(dat[,'mod_id_event'], concdate[,'mod_id_event']), 'date.time']
  } else {
    dt <- dat[,'date.time']
  }
  dat[,'date.time'] <- pkdata::parse_dates(fixDates(dt))

  if(!is.null(dem)) { # if using demographics, keep only conc data with matching demo record
    dat[,'mod_id'] <- dem[match(dat[,'mod_id_visit'], dem[,'mod_id_visit']), 'mod_id']
  }

  if(giveExample) {
    nodate <- dat[is.na(dat[,'date.time']),'mod_id_event']
    x <- dat[dat[,'mod_id_event'] %in% nodate, c('mod_id','mod_id_event')]
    cat('subjects with concentration missing from sample file\n')
    print(x[!duplicated(x[,'mod_id_event']),], row.names = FALSE)
  }
  # remove missing
  dat <- dat[!is.na(dat[,'date.time']),]
  dat <- dat[!is.na(dat[,'mod_id']),]
  
  # create event id
  idv <- as.numeric(unique(dat[,'mod_id_visit']))
  tid <- trunc(idv)
  sid <- split(idv, tid)
  nid <- cbind(orig=idv, eid=unsplit(sapply(sid, order), tid))
  dat[,'eid'] <- nid[match(dat[,'mod_id_visit'], nid[,'orig']), 'eid']
  multipleSets.id <- unique(dat[dat[,'eid'] > 1, 'mod_id'])
  cat(sprintf('%s subjects have multiple sets of concentration data\n', length(multipleSets.id)))
  cat(sprintf('%s total unique subjects ids (including multiple visits) currently in the concentration data\n', length(unique(dat[,'mod_id_visit']))))
  cat(sprintf('%s total unique subjects in the concentration data\n', length(unique(dat[,'mod_id']))))

  # use information to make rule based approach
  hasXid <- dat[,'mod_id'] %in% multipleSets.id
  nodupid <- unique(dat[!hasXid, 'mod_id_visit'])
  ddd <- dat[hasXid,]
  if(nrow(ddd)) {
    ddd <- ddd[order(ddd[,c('mod_id')]),]
    if(!is.null(checkDir)) {
      fn <- file.path(checkDir, sprintf(paste0(multsets_filename, drugname,"%s.csv"), Sys.Date()))
      msg <- sprintf('%s rows need review, see file %s\n', nrow(ddd), fn)
      writeCheckData(ddd, fn, msg)
    }

    if(is.na(lowerLimit)) {
      ddd[,'valid'] <- 1
    } else {
      ddd[,'valid'] <- +(ddd[,'conc.level'] >= lowerLimit)
    }
    tt <- tapply(ddd$valid, ddd$mod_id_visit, sum)
    tt <- data.frame(mod_id_visit=names(tt), num=tt)
    dd.x <- ddd[!duplicated(ddd$mod_id_visit), c('mod_id_visit', 'mod_id', 'eid')]
    dd.y <- merge(dd.x, tt)

    select.set <- do.call(rbind, lapply(split(dd.y, dd.y[,'mod_id']), function(i) {
      i[which.max(i[,'num']),]
    }))
    valid.multi.id <- c(nodupid, select.set[,'mod_id_visit'])
    dat <- dat[dat[,'mod_id_visit'] %in% valid.multi.id ,]
  } else {
    dat <- dat[dat[,'mod_id_visit'] %in% nodupid ,]
  }
  cat(sprintf('%s total unique subjects ids (after excluding multiple visits) in the concentration data\n', length(unique(dat[,'mod_id_visit']))))
  cat(sprintf('%s total unique subjects in the concentration data\n', length(unique(dat[,'mod_id']))))

  # check for duplicates
  if(!is.null(checkDir)) {
    cc <- do.call(paste, c(dat[,c('mod_id_visit','date.time')], sep = '|'))
    rnums <- which(cc %in% cc[duplicated(cc)])
    if(length(rnums)) {
      needfix <- dat[rnums,]
      needfix <- needfix[order(needfix[,'mod_id_event'], needfix[,'date.time']),]
      needfix <- cbind(needfix, flag = 'keep')
      nofix <- dat[-rnums,]
      fn <- file.path(checkDir, paste0('fail', faildupconc_filename, drugname, '.csv'))
      fixfn <- sub('fail', 'fix', fn)
      msg <- sprintf('%s rows need review, see file %s AND create %s\n', length(rnums), fn, fixfn)
      writeCheckData(needfix, fn, msg)
      if(file.access(fixfn, 4) != -1) {
        hasfix <- read.csv(fixfn, stringsAsFactors = FALSE)
        hasfix <- hasfix[hasfix[,'flag'] == 'keep',]
        if(nrow(hasfix)) {
          pd <- nrow(dat)
          dat <- rbind(nofix, hasfix[,names(nofix)])
          cat(sprintf('file %s read, %s records removed\n', fixfn, pd-nrow(dat)))
        }
      }
    }
  }

  # reorder by id, date.time
  dat[order(dat[,'mod_id_visit'], dat[,'date.time'], dat[,'conc.level']),]
}

infusionData_mod <- function(flow, mar, flowInt = 60, marInt = 15,
                             rateunit = 'mcg/hr', ratewgtunit = 'mcg/kg/hr', addWgt = NULL) {

  if(nrow(flow)){ # add check for >0 obs
    flow$maxint <- flowInt
  }

  if(nrow(mar)){ # add check for >0 obs
    mar$maxint <- marInt
    names(mar)[1] <- 'mod_id'
  }

  i1 <- flow[!is.na(flow$rate),]
  i2 <- mar[!is.na(mar$rate),]

  coi <- c("mod_id", "date.time", "infuse.dose", "unit", "rate", "weight", "maxint")
  if(nrow(i1)>0 & nrow(i2)>0) {
    infusionFile <- joinFlowMar(i1, i2)
  } else if (nrow(i1)>0 & nrow(i2)==0) { #only have i1 dat
    infusionFile <- i1
  } else if (nrow(i1)==0 & nrow(i2)>0) { #only have i2 dat
    i2[, setdiff(names(i1), names(i2))] <- NA
    i2 <- i2[, names(i1)]
    # no flow means no `maxint` in i1
    i2[,'maxint'] <- marInt
    infusionFile <- i2
  } else {
    warning("No Flow or MAR dosing information")
    jnk <- as.data.frame(matrix(NA, 1, length(coi)))
    names(jnk) <- coi
    return(jnk[FALSE,])
  }
  names(infusionFile) <- coi

  ## separate records with unit==rateunit (don't impute weight or multiply rate by weight)
  infusionFile1 <- infusionFile[infusionFile[,'unit'] == rateunit,]

  ## separate records with unit==ratewgtunit --> impute weight and multiply rate by weight
  infusionFile2 <- infusionFile[infusionFile[,'unit'] == ratewgtunit,]

  infFileWeight <- infusionFile[!is.na(infusionFile[,'weight']),c('mod_id','date.time','weight')]
  # supplement with additional weight if available
  if(!is.null(addWgt)) {
    infFileWeight <- rbind(infFileWeight, addWgt[,c('mod_id','date.time','weight')])
  }
  infFileWeight <- split(infFileWeight[,c('date.time','weight')], infFileWeight[,'mod_id'])

  # impute missing weight
  ix <- which(is.na(infusionFile2[,'weight']))
  wgtTimes <- infusionFile2[ix, 'date.time']
  wgtLocate <- match(infusionFile2[ix, 'mod_id'], names(infFileWeight))
  for(i in seq_along(ix)) {
    if(!is.na(wgtLocate[i])) {
      opt <- infFileWeight[[wgtLocate[i]]]
      infusionFile2[ix[i],'weight'] <- takeClosest(wgtTimes[i], opt[[1]], opt[[2]])
    }
  }
  # multiply hourly rate per weight unit by weight to get hourly rate
  infusionFile2[,'rate'] <- infusionFile2[,'rate'] * infusionFile2[,'weight']

  InfusionOut <- rbind(infusionFile1, infusionFile2)
  InfusionOut[order(InfusionOut$mod_id, InfusionOut$date.time),]
}

resolveDoseDups_mod <- function(dat, checkDir, drugname, faildupbol_filename) {
  # check if infusion and bolus data are present
  hasInf <- 'infuse.time.real' %in% names(dat)
  hasBol <- 'bolus.time' %in% names(dat)
  orderVars <- c('mod_id')

  if(hasInf) {
    coi <- c('mod_id','date.dose','infuse.time.real','infuse.time','infuse.dose')
    dkey <- do.call(paste, c(dat[,coi], sep = '|'))
    dupix <- !is.na(dat[,'infuse.time.real']) & duplicated(dkey)
    if(any(dupix)) {
      dat <- dat[!dupix,]
      rownames(dat) <- NULL
    }
    cat(sprintf('%s duplicated rows\n', sum(dupix)))

    info.x <- dat[!is.na(dat[,'infuse.time.real']),] # remove missing time
    key <- paste(info.x[,'mod_id'], info.x[,'infuse.time.real'], sep = '|')
    dupkey <- duplicated(key)
    if(any(dupkey)) {
      info.x$key <- +dupkey
      repflow <- unique(key[dupkey])
      posdup <- unique(info.x[key %in% repflow, 'mod_id'])
      dd <- info.x[info.x[,'mod_id'] %in% posdup,]
      dd <- dd[order(dd[,'mod_id'], dd[,'infuse.time.real']),]
      id.list <- unique(dd$mod_id)
      dd.resolved <- NULL
      for(i in id.list) {
        ddd <- dd[dd$mod_id==i,]
        ddd$remove <- 0
        rn <- rownames(ddd[ddd[,'key']==1,])
        # "2nd row duplicate" case
        if(length(rn) == 1 && match(rn, rownames(ddd)) == 2) {
          if(which.max(abs(ddd[3,'infuse.dose'] - ddd[1:2,'infuse.dose'])) == 2) {
            # rn is farther, remove it
            ddd[as.character(as.numeric(rn)), 'remove'] <- 1
          } else {
            # rn is closer, remove 'rn-1'
            ddd[as.character(as.numeric(rn)-1), 'remove'] <- 1
          }
        # "duplicate is not last row" case
        } else if(length(rn) == 1 && ddd[nrow(ddd), 'key'] == 0) {
          diffs <- abs(diff(ddd[as.character(as.numeric(rn)+c(-2,-1,0,1)), 'infuse.dose']))
          if(diffs[3] < diffs[1]) {
            ddd[as.character(as.numeric(rn)-1), 'remove'] <- 1 # row number 'rn' is correct dose, so remove rn-1
          } else {
            ddd[as.character(as.numeric(rn)), 'remove'] <- 1 # row number 'rn-1' is correct dose, so remove rn
          }
        } else {
          ddd[rn, 'remove'] <- 1
        }
        ddd <- ddd[ddd$remove==0, -ncol(ddd)]
        dd.resolved <- rbind(dd.resolved, ddd)
      }
      dd.resolved[['key']] <- NULL
      dat <- dat[!dat$mod_id %in% id.list, ]
      dat <- rbind(dat, dd.resolved)
      dat <- dat[order(dat[,'mod_id'], dat[,'infuse.time.real']),]
    }
    orderVars <- c(orderVars, 'infuse.time.real')
  }

  # check bolus dose as well
  if(hasBol) {
    dkey <- do.call(paste, c(dat[,c('mod_id','bolus.time')], sep = '|'))
    rnums <- which(dkey %in% dkey[!is.na(dat[,'bolus.time']) & duplicated(dkey)])
    if(!is.null(checkDir) && length(rnums)) {
      needfix <- dat[rnums,]
      needfix <- needfix[order(needfix[,'mod_id'], needfix[,'bolus.time']),]
      needfix <- cbind(needfix, flag = 'keep')
      nofix <- dat[-rnums,]
      fn <- file.path(checkDir, paste0('fail', faildupbol_filename, drugname, '.csv'))
      fixfn <- sub('fail', 'fix', fn)
      msg <- sprintf('%s rows need review, see file %s AND create %s\n', length(rnums), fn, fixfn)
      writeCheckData(needfix, fn, msg)
      if(file.access(fixfn, 4) != -1) {
        hasfix <- read.csv(fixfn, stringsAsFactors = FALSE)
        hasfix <- hasfix[hasfix[,'flag'] == 'keep',]
        if(nrow(hasfix)) {
          pd <- nrow(dat)
          dat <- rbind(nofix, hasfix[,names(nofix)])
          cat(sprintf('file %s read, %s duplicates removed\n', fixfn, pd-nrow(dat)))
        }
      }
    }
    orderVars <- c(orderVars, 'bolus.time')
  }

  # re-order by id and time variables
  dat[do.call(order, dat[,orderVars]),]
}

#' Write Check File as CSV
#'
#' A check file contains IDs and data that need to be manually verified.
#'
#' @param dat a data.frame
#' @param filename name of output file
#' @param msg character text to display
#' @param decor logical value to indicate decoration before and after the msg
#' @param \dots additional arguments to pass to \code{\link{write.csv}}
#' @keywords internal

writeCheckData <- function(dat, filename, msg, decor = TRUE, ...) {
  # merge dat with crosswalk to provide original IDs
  dat <- pullRealId(dat)
  dtext <- paste(c(rep('#', 25), '\n'), collapse = '')
  if(decor) cat(dtext)
  cat(msg)
  if(decor) cat(dtext)
  write.csv(dat, file = filename, row.names = FALSE, quote = FALSE, ...)
}

## This function assumes the E-prescription data has columns "ID", "RX_DOSE", "FREQUENCY", "ENTRY_DATE", "STRENGTH_AMOUNT", "DESCRIPTION".
## It also assumes all of the prescriptions are for only one drug.
## - For observations that are missing STRENGTH_AMOUNT, the strength in the DESCRIPTION can be used.
## - Observations that are missing strength are removed.
## - Frequency, dose, and strength are converted to numeric.
## - Daily dose is calculated as strength\*dose\*freq if dose <25
## - Daily dose is calculated as dose\*freq if dose>=25
## - Data is ordered by ID and ENTRY_DATE.
## - Duplicate daily doses on the same date for an ID are removed.

processErx <- function(rx, description=TRUE, strength_exclude="\\s*mg|\\(.*\\)", dose_exclude="\\s*(cap|capsule|tablet|tab|pill)[s]?") {
  str <- rx[,'STRENGTH_AMOUNT']

  ## If observations are missing STRENGTH_AMOUNT, use number from DESCRIPTION
  if(description == TRUE) {
    rx[,'DESCRIPTION'] <- tolower(rx[,'DESCRIPTION'])
    ix <- which(is.na(str) | str == '')
    desc <- rx[ix,'DESCRIPTION']
    m <- gregexpr("\\d+\\s*mg|\\d+\\.\\d+\\s*mg", desc)
    calcstr <- regmatches(desc, m)
    calcstr[lengths(calcstr) == 0] <- NA
    mix <- lengths(calcstr) > 1
    calcstr[mix] <- vapply(calcstr[mix], paste, character(1), collapse = ', ')
    str[ix] <- unlist(calcstr)
  }
  str[grep("c\\(", str)] <- NA
  str <- tolower(str)
  str <- gsub(strength_exclude, '', str)
  rx[,'strength'] <- nowarnnum(str)

  ## Drop observations missing strength
  rx <- rx[!is.na(rx[,'strength']),]

  ## Get numeric frequency
  freq <- unique(rx[,'FREQUENCY'])
  freqstnd <- stdzFreq(freq)
  freqnum <- freqNum(freqstnd)
  ix <- match(rx[,'FREQUENCY'], freq)
  rx[,'freq.standard'] <- freqstnd[ix]
  rx[,'freq.num'] <- freqnum[ix]

  ## Get numeric dose
  dose <- tolower(rx[,'RX_DOSE'])
  dose <- gsub(dose_exclude, "", dose)
  rx[,'dose'] <- nowarnnum(dose)

  ## Calculate daily dose
  ## If dose is <25, consider it to be number of pills
  ## If dose is >=25, consider it to be dose intake
  dailydose <- rx[,'dose'] * rx[,'freq.num']
  ix <- which(rx[,'dose'] < 25)
  dailydose[ix] <- dailydose[ix] * rx[ix,'strength']
  rx[,'daily.dose'] <- dailydose

  ## Order by ID and date
  rx <- rx[order(rx[,'ID'], rx[,'ENTRY_DATE']),]

  ## Get rid of duplicate daily.dose on the same date for an ID
  id_date_dose <- do.call(paste, c(rx[,c('ID','ENTRY_DATE','daily.dose')], sep = '|'))
  rx <- rx[!duplicated(id_date_dose),]
  rx
}

## This function takes the output from `processErx` and does some additional processing.
## - The word "and" used to separate doses in RX_DOSE is changed to "+"
## - If doses are separated by plus signs, commas, dashes, or slashes, and the number of doses matches the frequency, then the individual doses are added together and daily dose is calculated as strength*dose
## - Units such as "mg" are removed from RX_DOSE and daily dose is calculated as dose*frequency in these cases

processErxAddl <- function(processed) {
  ## Change "and" in RX_DOSE to "+"
  processed[,'RX_DOSE'] <- gsub("\\s+and\\s+", "+", processed[,'RX_DOSE'])
  processed[,'num_doses'] <- NA
  processed[,'num_freqs'] <- NA
  processed[,'FREQUENCY'] <- tolower(processed[,'FREQUENCY'])
  ## punctuation characters, plus | comma | hyphen | slash (PCHS)
  pchs <- "\\+|\\,|-|/"
  ix <- which(is.na(processed[,'daily.dose']) & grepl(pchs, processed[,'RX_DOSE']))
  fix <- processed[ix,'FREQUENCY']
  num_freqs <- rep(NA, length(ix))
  ## If frequencies are separated by punctuation characters, get the number of frequencies. Otherwise, use the numeric frequency as number of frequencies.
  hasSplitFreq <- grepl(pchs, fix) & grepl("am|pm|brkfst|lunch|dinner", fix)
  num_freqs[!hasSplitFreq] <- processed[ix[!hasSplitFreq],'freq.num']
  num_freqs[hasSplitFreq] <- lengths(strsplit(fix[hasSplitFreq], pchs))
  ## If doses are separated by punctuation characters, get the number of doses
  splitDose <- strsplit(processed[ix,'RX_DOSE'], pchs)
  num_doses <- lengths(splitDose)
  processed[ix,'num_doses'] <- num_doses
  processed[ix,'num_freqs'] <- num_freqs
  ## If the number of doses == number of frequencies, calculate daily dose as strength*dose.
  nfix <- which(!is.na(num_freqs) & num_doses == num_freqs)
  doses <- splitDose[nfix]
  dose <- vapply(doses, function(i) sum(as.numeric(i), na.rm = TRUE), numeric(1))
  processed[ix[nfix],'dose'] <- dose
  processed[ix[nfix],'daily.dose'] <- dose * processed[ix[nfix],'strength']

  ## Get rid of units (e.g., mg) in RX_DOSE
  ix <- which(is.na(processed[,'daily.dose']) & grepl('mg', processed[,'RX_DOSE']))
  dose <- as.numeric(sub('mg', '', processed[ix,'RX_DOSE']))
  processed[ix,'dose'] <- dose
  processed[ix,'daily.dose'] <- dose * processed[ix,'freq.num']
  processed
}

validateColumns <- function(df, columnSpecs, defaultSpecs = list()) {
  # KEY = colname(s)
  # if default is NULL, not required
  # if default is NA, required
  n <- names(df)
  u <- names(columnSpecs)
  if(length(defaultSpecs) == 0) {
    defaultSpecs <- as.list(rep(NA, length(columnSpecs)))
    names(defaultSpecs) <- u
  }
  x <- names(defaultSpecs)
  cst <- function(t, v) {
    sprintf('%s"%s"', t, paste(v, collapse = '", "'))
  }
  errors <- character(4)
  # user should provide named list, or list of given length
  if(is.null(u)) {
    if(length(columnSpecs) == length(x)) {
      u <- x
      names(columnSpecs) <- x
    } else {
      errors[1] <- cst('column specification is incorrect; please identify all columns: ', x)
    }
  }
  # user should not provide unexpected columns
  bad_col <- setdiff(u, x)
  if(length(bad_col)) {
    errors[2] <- cst('column specification is incorrect; the following column(s) should not be present: ', bad_col)
  }
  # provide defaults, including NULL/NA
  add_col <- setdiff(x, u)
  columnSpecs[add_col] <- defaultSpecs[add_col]
  # safely remove NULL
  columnSpecs <- columnSpecs[lengths(columnSpecs, FALSE) > 0L]
  u <- names(columnSpecs)
  # require NA
  req_col <- u[is.na(sapply(columnSpecs, `[`, 1))]
  if(length(req_col)) {
    errors[3] <- cst('column specification is incorrect; please identify the following columns: ', req_col)
  }
  # check for missing columns
  mycols <- unlist(columnSpecs)
  mycols <- mycols[!is.na(mycols)]
  miss_col <- setdiff(mycols, c(n, seq_along(n)))
  if(length(miss_col)) {
    errors[4] <- cst('data set is missing expected columns; the following column(s) are missing: ', miss_col)
  }
  err <- paste(errors[errors != ''], collapse = '\n  ')
  if(err != '') stop(err)
  # convert any numeric columns into names
  for(i in seq_along(columnSpecs)) {
    csix <- match(columnSpecs[[i]], seq_along(n))
    columnSpecs[[i]][!is.na(csix)] <- n[csix[!is.na(csix)]]
  }
  columnSpecs
}

read <- function(file, readFun = NULL, readArgs = list()) {
  if(inherits(file, 'data.frame')) {
    return(file)
  }
  if(file.access(file, 4) == -1) {
    stop(sprintf('file [%s] is not accessible', file))
  }
  if(is.null(readFun)) {
    ext <- gsub('.*[.]', '', tolower(basename(file)))
    readFun <- switch(ext,
      rdata =,
      rda = 'load',
      rds = 'readRDS',
      csv = 'read.csv',
      tab = 'read.delim',
      'read.table'
    )
  }
  if(readFun == 'load') {
    e <- new.env()
    load(file, envir = e)
    vars <- names(e)
    isDF <- vapply(vars, function(i) inherits(e[[i]], 'data.frame'), logical(1))
    if(sum(isDF) == 0) {
      stop(sprintf('file [%s] does not contain any data.frame objects', file))
    }
    if(sum(isDF) > 1) {
      warning(sprintf('file [%s] contains multiple data.frame objects; only the first will be used', file))
    }
    out <- e[[vars[isDF][1]]]
  } else if(readFun == 'readRDS') {
    out <- readRDS(file)
  } else {
    out <- do.call(readFun, c(list(file), readArgs))
  }
  if(!inherits(out, 'data.frame')) {
    stop(sprintf('file [%s] does not produce a data.frame object', file))
  }
  out
}
