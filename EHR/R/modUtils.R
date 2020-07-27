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
#' @name mod-internal
#' @aliases colflow_mod findMedFlowRow_mod flowData_mod concData_mod
#' infusionData_mod resolveDoseDups_mod
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
  # require mod_id_visit|date.time|weight|rate|final.units
  # create idnum
  # combine it if input is list
  if(class(dat) == 'list') {
    dat <- do.call(rbind, dat)
  }
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
      msg <- sprintf('%s rows need review, see file %s AND create %s\n', length(rnums), fn, fixfn)
      writeCheckData(needfix, fn, msg)
      if(file.access(fixfn, 4) != -1) {
        hasfix <- read.csv(fixfn, stringsAsFactors = FALSE)
        if(nrow(hasfix)) {
          dat <- rbind(dat, hasfix[,names(dat)])
          cat(sprintf('file %s read with failures replaced\n', fixfn))
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
  # reorder by id, date.time
  dat[order(dat[,'mod_id_visit'], dat[,'date.time']),]
}

concData_mod <- function(dat, sampFile, lowerLimit, drugname, giveExample = TRUE,
                         checkDir, dem=NULL,
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
  dt <- parse_dates(fixDates(concdate[,'date.time']))

  fn <- file.path(checkDir, paste0('fail', failmissconc_filename, drugname, '.csv'))
  fixfn <- sub('fail', 'fix', fn)
  rnums <- which(is.na(dt))
  if(length(rnums)) {
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
  dat[,'date.time'] <- parse_dates(fixDates(dt))

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
  ddd <- dat[dat[,'mod_id'] %in% multipleSets.id,]
  ddd <- ddd[order(ddd[,c('mod_id')]),]
  fn <- file.path(checkDir, sprintf(paste0(multsets_filename, drugname,"%s.csv"), Sys.Date()))
  msg <- sprintf('%s rows need review, see file %s\n', nrow(ddd), fn)
  writeCheckData(ddd, fn, msg)

  ddd[,'valid'] <- +(ddd[,'conc.level'] >= lowerLimit)
  tt <- tapply(ddd$valid, ddd$mod_id_visit, sum)
  tt <- data.frame(mod_id_visit=names(tt), num=tt)
  dd.x <- ddd[!duplicated(ddd$mod_id_visit), c('mod_id_visit', 'mod_id', 'eid')]
  dd.y <- merge(dd.x, tt)

  select.set <- do.call(rbind, lapply(split(dd.y, dd.y[,'mod_id']), function(i) {
    i[which.max(i[,'num']),]
  }))
  nodupid <- unique(dat[!(dat[,'mod_id'] %in% multipleSets.id), 'mod_id_visit'])
  valid.multi.id <- c(nodupid, select.set[,'mod_id_visit'])
  dat <- dat[dat[,'mod_id_visit'] %in% valid.multi.id ,]
  cat(sprintf('%s total unique subjects ids (after excluding multiple visits) in the concentration data\n', length(unique(dat[,'mod_id_visit']))))
  cat(sprintf('%s total unique subjects in the concentration data\n', length(unique(dat[,'mod_id']))))

  # check for duplicates
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

  # reorder by id, date.time
  dat[order(dat[,'mod_id_visit'], dat[,'date.time'], dat[,'conc.level']),]
}

infusionData_mod <- function(flow, mar, flowInt = 60, marInt = 15,
                             rateunit = 'mcg/hr', ratewgtunit = 'mcg/kg/hr') {
  
  flow$maxint <- flowInt
  mar$maxint <- marInt
  i1 <- flow[!is.na(flow$rate),]
  i2 <- mar[!is.na(mar$rate),]
  infusionFile <- combine(i1, i2)

  ## separate records with unit==rateunit (don't impute weight or multiply rate by weight)
  infusionFile1 <- infusionFile[infusionFile[,'unit'] == rateunit,]

  ## separate records with unit==ratewgtunit --> impute weight and multiply rate by weight
  infusionFile2 <- infusionFile[infusionFile[,'unit'] == ratewgtunit,]

  # impute missing weight
  ix <- which(is.na(infusionFile2[,'weight']))
  lastid <- -1
  for(i in seq_along(ix)) {
    row <- infusionFile2[ix[i], c('mod_id','date.time')]
    if(row[[1]] != lastid) {
      opt <- infusionFile[infusionFile[,'mod_id'] == row[[1]], c('date.time','weight')]
      lastid <- row[[1]]
    }
    infusionFile2[ix[i],'weight'] <- takeClosest(row[[2]], opt[[1]], opt[[2]])
  }
  # multiply hourly rate per weight unit by weight to get hourly rate
  infusionFile2[,'rate'] <- infusionFile2[,'rate'] * infusionFile2[,'weight']

  InfusionOut <- rbind(infusionFile1, infusionFile2)
  InfusionOut[order(InfusionOut$mod_id, InfusionOut$date.time),]
}

resolveDoseDups_mod <- function(dat, checkDir, drugname, faildupbol_filename) {
  dkey <- do.call(paste, c(dat[,c(1:5)], sep = '|'))
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
      if(length(rn) == 1 && ddd[nrow(ddd), 'key'] == 0) {
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
  # check bolus dose as well
  dkey <- do.call(paste, c(dat[,c('mod_id','bolus.time')], sep = '|'))
  rnums <- which(dkey %in% dkey[!is.na(dat[,'bolus.time']) & duplicated(dkey)])
  if(length(rnums)) {
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
  dat[order(dat[,'mod_id'], dat[,'infuse.time.real'], dat[,'bolus.time']),]
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
