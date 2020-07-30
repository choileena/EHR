#' Internal functions for buildDose process
#'
#' These internal functions aid the \code{\link{buildDose}} function and process.
#' 
#' \code{tstrsplit2}: a wrapper for the \code{\link[data.table]{tstrsplit}} function in \pkg{data.table} 
#' with some additional formatting 
#' 
#' \code{convert}: formats final results that are returned in \code{\link{buildDose}}, including 
#' separating the extracted expression from its start and stop position
#' 
#' \code{entVal}: isolates the extracted expression from a string of the format 
#' "extracted expression::start position::stop position"
#' 
#' \code{entStart}: isolates the start position from a string of the format 
#' "extracted expression::start position::stop position"
#' 
#' \code{entStop}: isolates the stop position from a string of the format 
#' "extracted expression::start position::stop position"
#' 
#' \code{remainder}: performs some formatting for paths containing NA values
#' 
#' \code{getCombosIx}: forms the individual paths within the \code{getPaths} function
#' 
#' \code{getPaths}: builds paths from entity pairings using other helper function \code{getCombosIx}
#' 
#' \code{getOpt}: takes in the possible paths as well as the cost computations from \code{getCosts}, 
#' combines costs at the path level using the helper functions \code{remainder} and \code{getPaths}, 
#' and returns the path with the minimum cost
#' 
#' \code{getCosts}: computes the distance for each path based on the distance method specified in \code{buildDose} 
#' 
#' \code{findOptPath}: uses the helper functions \code{getCosts} and \code{getOpt} to 
#' determine the optimal path based on the selected distance method
#' 
#' \code{stdRoute}: standardize the "route" entity. This function is also used in the 
#' \code{collapseDose} function and process.
#' 
#' \code{grp}: groups drug entities based on a particular anchor point (e.g. strength 
#' or dose)
#' 
#' \code{anchorByStrength}: forms subgroups of entities by anchoring to strength. This is done within existing groups 
#' which are anchored by drug name
#' 
#' \code{combineGroups}: determines how to form groups of the entities based on the complexity of the extraction (i.e., 
#' number of extractions for each entity)
#' 
#' \code{makeCombos}: takes all extracted entities and puts them into groups based on position and minimum cost of paths. 
#' This is the highest level function within \code{\link{buildDose}}; it calls other helper functions including \code{combineGroups} 
#' and \code{anchorByStrength}.
#'
#' \code{findRareValues}: find rare values for each column in a data.frame; rare defaults to less than two percent.
#'
#' @name build-internal
#' @aliases tstrsplit2 convert entVal entStart entStop
#' remainder getCombosIx getPaths getOpt getCosts findOptPath
#' stdRoute grp anchorByStrength combineGroups makeCombos findRareValues
#' @keywords internal
NULL

tstrsplit2 <- function(x, split) {
  res <- data.table::tstrsplit(x, split)
  if(length(res) == 0) {
    tmp <- rep(NA, length(x))
    res <- list(tmp, tmp, tmp)
  }
  res
}

convert <- function(x) {
  cnames <- setdiff(names(x), c('filename'))
  for(i in seq_along(cnames)) {
    newcols <- c(cnames[i], paste(cnames[i], c('A','B'), sep = '.'))
    x[, (newcols) := tstrsplit2(get(cnames[i]), "::")]
  }
  chng <- grep('A|B$', names(x))
  x[,(chng):= lapply(.SD, as.numeric), .SDcols = chng]
  x
}

entVal <- function(str) sub('([^:]*)::.*::.*', '\\1', str)
entStart <- function(str) as.numeric(sub('.*::(.*)::.*', '\\1', str))
entStop <- function(str) as.numeric(sub('.*::.*::(.*)', '\\1', str))

remainder <- function(d, ix) {
  d[is.na(d)] <- -999
  r <- d[rep(ix, nrow(d)),]
  # NAs shouldn't be mutually exclusive, but it may not be worth it
  # find 2x4 example
  # unname(rowSums(r[rep(1,nrow(d)),] == d, na.rm = TRUE) == 0)
  unname(rowSums(r[rep(1,nrow(d)),] == d) == 0)
}

getCombosIx <- function(m, v, e) {
  c1 <- which(rowSums(m[,v,drop=FALSE]) == length(v) & seq(nrow(m)) > max(v))
  if(length(c1)) {
    return(lapply(c1, function(i) list(i, getCombosIx(m, c(v, i), e))))
  }
  e$paths[[v[1]]] <- append(e$paths[[v[1]]], list(v))
  return(NULL)
}

getPaths <- function(m) {
  e <- new.env()
  e$paths <- vector('list', nrow(m))
  for(i in seq(nrow(m))) {
    getCombosIx(m, i, e)
  }
  do.call(c, e$paths)
}

getOpt <- function(x, cost, useGreedy = 1e8) {
  nr <- nrow(x)
  k <- max(apply(x, 2, function(i) length(unique(i))))
  nck <- choose(nr, k)

  # too many choices are problematic
  if(nck > useGreedy) {
    # greedy algorithm
    xx <- x[order(cost),]
    path <- 1
    repeat {
      pick <- which(rowSums(vapply(path, remainder, logical(nr), d = xx)) == length(path))[1]
      if(is.na(pick)) break
      path <- c(path, pick)
    }
    return(xx[path,])
  }

  # remainder needs NA values to be at unique positions
  m <- matrix(FALSE, nr, nr)
  for(i in seq(nr)) {
    m[,i] <- remainder(x, i)
  }
  hp <- getPaths(m)
  # reset those unique positions to NA
  x[x < 0] <- NA

  # restrict to unique paths
  l <- length(hp)
  path_uniq <- character(l)
  for(i in seq(l)) {
    path_i <- as.character(x[hp[[i]],])
    path_uniq[i] <- paste(path_i, collapse = '^')
  }
  hp <- hp[!duplicated(path_uniq)]

  # faster version of `sort`
  helperSort <- function(x, uniq = FALSE) {
    y <- x[!is.na(x)]
    if(uniq) y <- unique(y)
    y[order(y, method = 'radix')]
  }
  uniqElements <- function(df, uniq = FALSE) {
    lapply(df, helperSort, uniq)
  }
  el <- uniqElements(x, TRUE)
  # a valid path must have a minimum number of rows
  minrow <- max(vapply(el, length, numeric(1)))

  # restrict to valid paths
  l <- length(hp)
  path_diff <- logical(l)
  for(i in seq(l)) {
    path_i <- x[hp[[i]],]
    if(nrow(path_i) >= minrow) {
      pe <- uniqElements(path_i)
      path_diff[i] <- identical(el, pe)
    }
  }
  khp <- hp[path_diff]

  # calculate path distance
  l <- length(khp)
  pd <- rep(Inf, l)
  # use mean or sum?
  for(i in seq(l)) {
    pd[i] <- mean(cost[khp[[i]]])
  }
  top2 <- order(pd)[1:2]
  path <- khp[[top2[1]]]
  x[path,]
}

getCosts <- function(byBeg, byEnd, na_penalty = 32) {
  ent <- sort(names(byBeg))
  byBeg <- byBeg[,ent]
  byEnd <- byEnd[,ent]
  # form entity pair names, ie "dose+freq"
  pairs <- unlist(lapply(ent, function(i) paste(i, ent, sep = '+')))

  nc <- ncol(byBeg)
  fm <- matrix(0,nc,nc)
  ix2 <- which(upper.tri(fm))
  ix1 <- which(lower.tri(fm))
  pn <- pairs[ix1]

  byBeg[byBeg < 0] <- NA
  byEnd[byEnd < 0] <- NA

  t1 <- matrix(Inf, nrow(byBeg), length(pn))
  t2 <- matrix(Inf, nrow(byBeg), length(pn))
  colnames(t1) <- pn
  colnames(t2) <- pn
  neg_mag <- getOption('ehr.neg_penalty')
  if(is.null(neg_mag)) neg_mag <- 0.5
  neg_penalty <- na_penalty * neg_mag
  for(i in seq_along(pn)) {
    if(pn[i] == 'dose+freq' ) {
      val <- byBeg[,'freq'] - byBeg[,'dose']
      nv <- !is.na(val) & val < 0
      val[nv] <- abs(val[nv]) + neg_penalty
      val2 <- byBeg[,c('freq','dose')] - byEnd[,c('dose','freq')]
      val2[,2] <- val2[,2] * -1
      nv <- !is.na(val2) & val2 < 0
      val2[nv] <- abs(val2[nv]) + neg_penalty
    } else {
      e1 <- sub('[+].*', '', pn[i])
      e2 <- sub('.*[+]', '', pn[i])
      val <- abs(byBeg[,e1] - byBeg[,e2])
      val2 <- abs(byBeg[,c(e1,e2)] - byEnd[,c(e2,e1)])
    }
    val[is.na(val)] <- na_penalty
    val2[is.na(val2)] <- na_penalty
    t1[,i] <- val
    t2[,i] <- apply(val2, 1, min)
    m1 <- apply(byBeg, 1, min, na.rm = TRUE)
    m2 <- apply(byEnd, 1, max, na.rm = TRUE)
    t0 <- m2 - m1 + rowSums(is.na(byBeg)) * na_penalty
  }
  if(nc > 2) {
    t3 <- t(apply(t1, 1, function(i) sort(i)[-nc]))
    t4 <- t(apply(t2, 1, function(i) sort(i)[-nc]))
  } else {
    t3 <- t1
    t4 <- t2
  }
  t5 <- t3^2
  t6 <- t4^2
  t7 <- cbind(t3[,1], t3)
  t8 <- cbind(t4[,1], t4)
  dd <- data.frame(
    minMax = t0,
    byBeg = rowSums(t1),
    byEnd = rowSums(t2),
    sumBeg = rowSums(t3),
    sumEnd = rowSums(t4),
    sumBegSq = rowSums(t5),
    sumEndSq = rowSums(t6),
    minEntBeg = rowSums(t7),
    minEntEnd = rowSums(t8)
  )
  dd
}

# strength/dosestr should be established
findOptPath <- function(indat, est) {
  # potential options
  meth <- getOption('ehr.dist_method')
  if(is.null(meth)) meth <- 'minEntEnd'
  na_pen <- getOption('ehr.na_penalty')
  if(is.null(na_pen)) na_pen <- ((meth == 'minMax') + 1) * 32
  greedThresh <- getOption('ehr.greedy_threshold')
  if(is.null(greedThresh)) greedThresh <- 1e8
  # end of options
  ix <- match(est, names(indat))
  size <- vapply(indat, length, numeric(1))
  ix <- unique(c(ix, which(size == 0)))
  entityOutPath <- indat[ix]
  entityInPath <- indat[-ix]
  if(length(entityInPath) == 1) {
    ans <- vector('list', length(entityInPath[[1]]))
    entity <- names(entityInPath)
    for(i in seq_along(ans)) {
      ans[[i]] <- entityOutPath
      keep <- entityInPath[[entity]][i]
      ans[[i]][[entity]] <- keep
    }
    return(ans)
  }
  maxsize <- max(size[names(entityInPath)])
  for(i in seq_along(entityInPath)) {
    reqNA <- unname(maxsize - size[names(entityInPath)[i]])
    useNA <- character(0)
    if(reqNA > 0) {
      fakeLoc <- -999 + seq_len(reqNA)
      useNA <- paste0('missing::', fakeLoc, '::', fakeLoc)
    }
    entityInPath[[i]] <- c(entityInPath[[i]], NA, useNA)
  }
  eg <- expand.grid(entityInPath)
  hasMiss <- apply(eg, 2, grepl, pattern = '^missing')
  eg <- eg[rowSums(is.na(eg) | hasMiss) != ncol(eg),, drop = FALSE]
  egloc <- as.data.frame(apply(eg, 2, entStart))
  eglocEnd <- as.data.frame(apply(eg, 2, entStop))
  # create cost for each pair
  gc <- getCosts(egloc, eglocEnd, na_pen)
  costs <- gc[,meth]
  loc <- lapply(entityInPath, entStart)
  # form every valid combination and take minimum group
  best <- getOpt(egloc, costs, greedThresh)
  nr <- nrow(best)
  ans <- vector('list', nr)
  for(i in seq_along(ans)) {
    ans[[i]] <- entityOutPath
    for(j in seq_along(entityInPath)) {
      entity <- names(entityInPath)[j]
      mix <- match(best[i,entity], loc[[entity]])
      keep <- entityInPath[[entity]][mix]
      if(grepl('^missing::-9', keep)) keep <- NA
      ans[[i]][[entity]] <- keep
    }
  }
  ans
}

stdRoute <- function(x) {
  rte <- gsub('[. ]', '', tolower(x))
  rte[grep('skin', rte)] <- 'transdermal'
  rte[grep('iv|intravenous', rte)] <- 'iv'
  rte[grep('mouth|oral|po', rte)] <- 'orally'
  rte[grep('subcut|sq', rte)] <- 'sq'
  rte
}

# group item by point-of-interest (strength)
grp <- function(p, brks) {
  cut(p, c(-Inf,brks,Inf), labels = FALSE, right = FALSE) - 1
}

anchorByStrength <- function(poi, loc, dat, str, doseOpt, dosestr, dsgrp) {
  lp <- length(poi)
  if(lp == 1 && is.na(poi)) {
    grps <- dat
    grps[['strength']] <- character(0)
  } else {
    grps <- vector('list', lp + 1)
    info <- lapply(loc, grp, poi)
    # dose must match doseOpt
    if('dose' %in% names(info) && length(info[['dose']])) {
      baddose <- which(!info[['dose']] %in% doseOpt)
      while(length(baddose)) {
        info[['dose']][baddose] <- info[['dose']][baddose] - 1
        baddose <- which(!info[['dose']] %in% doseOpt)
      }
    }
    for(i in seq_along(grps)) {
      grps[[i]] <- vector('list', length(dat))
      for(j in seq_along(dat)) {
        if(length(dat[[j]])) {
          grps[[i]][[j]] <- dat[[j]][info[[j]] == (i - 1)]
        } else {
          grps[[i]][[j]] <- character(0)
        }
      }
      names(grps[[i]]) <- names(dat)
      if(!is.null(dosestr)) {
        grps[[i]][['dosestr']] <- character(0)
      }
      if(i == 1) {
        grps[[i]][['strength']] <- character(0)
      } else if((i - 1) %in% dsgrp) {
        grps[[i]][['strength']] <- character(0)
        grps[[i]][['dosestr']] <- dosestr[match(i-1, dsgrp)]
      } else {
        # if it wasn't in dsgrp, it must be in doseOpt
        # but it can't be group 1 (ie zero)
        strgrp <- match(i - 1, doseOpt) - 1
        grps[[i]][['strength']] <- str[strgrp]
      }
    }
  }
  grps
}

combineGroups <- function(dat, layers = NULL) {
  ix <- match(c('drugname','strength'), names(dat))
  dn <- dat[[ix[1]]]
  str <- dat[[ix[2]]]
  dat <- dat[-ix]
  loc <- lapply(dat, entStart)
  poi <- entStart(str)
  doseOpt <- seq_along(poi)
  dsgrp <- numeric(0)
  entities <- 'strength'
  if(!is.null(layers)) {
    ds <- entStart(layers)
    poi <- sort(unique(c(poi, ds)))
    dsgrp <- match(ds, poi)
    doseOpt <- which(!poi %in% ds)
    entities <- c(entities, 'dosestr')
  }
  doseOpt <- c(0, doseOpt)
  # standardize route
  route <- NULL
  ix <- match('route', names(dat))
  if(!is.na(ix) && length(dat[['route']])) {
    rte <- entVal(dat[['route']])
    # one form of route
    if(length(unique(stdRoute(rte))) == 1) {
      route <- dat[[ix]]
      rteloc <- loc[[ix]]
      dat <- dat[-ix]
      loc <- loc[-ix]
      entities <- c(entities, 'route')
    }
  }
  cols <- c(entities, names(dat))
  if(length(unlist(loc)) == 0) {
    if(!is.null(route)) {
      dat[['route']] <- route
    }
    for(i in seq_along(dat)) {
      if(length(dat[[i]]) == 0) dat[[i]] <- NA
    }
    # if multiple routes, group by strength
    if(!is.null(route) && length(route) > 1) {
      loc$route <- rteloc
      grps <- anchorByStrength(poi, loc, dat, str, doseOpt, layers, dsgrp)
      ingrp1 <- sum(vapply(grps[[1]], length, numeric(1)))
      if(ingrp1 == 0) {
        grps <- grps[-1]
      }
      # single or NULL route
    } else {
      if(length(str) == 0) str <- NA
      dat[['strength']] <- str
      if(!is.null(layers)) {
        dat[['dosestr']] <- layers
      }

      newdat <- as.data.frame(dat)
      # possible for drugname to be only entity
      if(nrow(newdat) == 1 && sum(!is.na(newdat[1,])) == 0) {
        return(NULL)
      }
      return(cbind(drugname = dn, newdat))
    }
  } else {
    grps <- anchorByStrength(poi, loc, dat, str, doseOpt, layers, dsgrp)
    # group 1 values
    ingrp1 <- sum(vapply(grps[[1]], length, numeric(1)))

    # special unique route case
    # this should guarantee one route per group
    if(!is.null(route)) {
      rtegrp <- grp(rteloc, poi) + 1
      if(1 %in% rtegrp) ingrp1 <- ingrp1 + 1
      for(i in seq_along(grps)) {
        closestRte <- which(rtegrp == i)[1]
        if(is.na(closestRte)) {
          closestRte <- which.min(abs(rteloc - poi[i-1]))
        }
        if(length(closestRte) == 0) closestRte <- 1
        grps[[i]][['route']] <- route[closestRte]
      }
    }
    # remove group 1 if nothing found
    if(ingrp1 == 0) {
      grps <- grps[-1]
    }
  }

  info <- sapply(grps, function(i) vapply(i, length, numeric(1)))
  # route is a special case
  if('route' %in% rownames(info)) {
    rtecheck <- info['route',] > 1
  } else {
    rtecheck <- FALSE
  }
  cs <- colSums(info > 1)
  done <- grps[cs == 0]
  part <- grps[cs == 1 & rtecheck]
  work <- grps[cs > 1 | (cs == 1 & !rtecheck)]
  # only one entity with discrepancy
  for(i in seq_along(part)) {
    rp <- max(info[,cs == 1,drop = FALSE][,i])
    anchoropt <- (!is.null(layers) && length(part[[i]][['dosestr']])) + 1
    anchorpnt <- c('strength','dosestr')[anchoropt]
    s <- part[[i]][[anchorpnt]]
    part[[i]][[anchorpnt]] <- rep(s, rp)
  }
  if(length(work)) {
    work <- lapply(work, function(i) {
      findOptPath(i, est = entities)
    })
    work <- do.call(c, work)
  }
  done <- c(done, part, work)
  res <- vector('list', length(done))
  for(i in seq_along(done)) {
    for(j in seq_along(done[[i]])) {
      if(length(done[[i]][[j]]) == 0) {
        done[[i]][[j]] <- NA
      }
    }
    res[[i]] <- as.data.frame(done[[i]])[,cols]
  }
  ans <- do.call(rbind, res)
  # reorder by position
  if(nrow(ans) > 1) {
    posmat <- t(apply(apply(ans, 2, entStart), 1, sort, na.last = TRUE))
    ix <- do.call(order, as.list(as.data.frame(posmat)))
    ans <- ans[ix,]
    rownames(ans) <- NULL
  }
  cbind(drugname = dn, ans)
}

makeCombos <- function(row, gap, preserve = NULL) {
  # NSE fix for R CMD CHECK
  ..pcols <- NULL
  ..xnames <- NULL
  rm(..pcols, ..xnames)
  # end
  xnames <- names(row)
  if(!is.null(preserve)) {
    preserve <- intersect(xnames, preserve)
    pcols <- match(preserve, xnames)
    xp <- row[, ..pcols]
    row <- row[,-..pcols]
  } else {
    xp <- NULL
  }
  fn <- row[,1]
  entities <- lapply(row[,-1], function(i) strsplit(i, "`")[[1]])
  s.i <- lapply(entities, entStart)
  loc <- sort(unlist(s.i))
  grps <- unique(unname(loc[c(TRUE, diff(loc) > gap)]))
  lg <- length(grps)
  if('dosestr' %in% xnames) {
    hasDS <- TRUE
    dsix <- match('dosestr', names(entities))
    dsEntity <- entities[[dsix]]
    ds.s.i <- s.i[[dsix]]
    entities <- entities[-dsix]
    s.i <- s.i[-dsix]
  } else {
    hasDS <- FALSE
    dsEntity <- character(0)
  }
  ent.names <- names(entities)
  nEntities <- length(entities)

  clusters <- vector('list', lg)
  for(i in seq_along(clusters)) {
    clusters[[i]] <- setNames(vector('list', nEntities), ent.names)
  }
  ent.len <- vapply(entities, length, numeric(1))
  for(i in seq_along(entities)) {
    ent.name <- ent.names[i]
    l <- ent.len[i]
    # entity doesn't exist in any cluster
    if(l == 0) {
      for(j in seq_along(clusters)) {
        clusters[[j]][[i]] <- character(0)
      }
    } else {
      s.g <- cut(s.i[[i]], c(grps,Inf), labels = FALSE, right = FALSE)
      for(j in seq_along(clusters)) {
        opts <- numeric(0)
        # copy drugname to all clusters
        if(l == 1 && ent.name == 'drugname') {
          opts <- entities[[i]]
          # copy strength to clusters >=
        } else if(l == 1 && ent.name == 'strength' && !hasDS && j >= s.g) {
          opts <- entities[[i]]
        } else {
          opts <- entities[[i]][s.g == j]
        }
        if(length(opts) == 0) opts <- character(0)
        clusters[[j]][[i]] <- opts
      }
    }
  }
  # dosestr
  dsCluster <- NULL
  if(hasDS) {
    dsCluster <- vector('list', lg)
    s.g <- cut(ds.s.i, c(grps,Inf), labels = FALSE, right = FALSE)
    # special conditions of length(dsEntity) == 1 and no strength
    if(ent.len['strength'] == 0 && ent.len['dose'] == 0 && length(dsEntity) == 1) {
      for(j in seq_along(dsCluster)) {
        opts <- dsEntity[j >= s.g]
        if(length(opts)) dsCluster[[j]] <- opts
      }
    } else {
      for(j in seq_along(dsCluster)) {
        opts <- dsEntity[s.g == j]
        if(length(opts)) dsCluster[[j]] <- opts
      }
    }
  }

  allGroups <- vector('list', lg)
  for(i in seq_along(allGroups)) {
    tmp <- combineGroups(clusters[[i]], dsCluster[[i]])
    if(length(tmp)) allGroups[[i]] <- tmp
    if(length(tmp) && hasDS && is.null(dsCluster[[i]])) {
      allGroups[[i]][,'dosestr'] <- NA
    }
  }
  allGroups <- cbind(fn, do.call(rbind, allGroups))
  if(!is.null(xp)) {
    allGroups <- cbind(allGroups, xp)
  }
  unique(allGroups[,..xnames])
}

## question of interest ##
# given `rep(1:10, 1:10)`, which values are significantly rare?
#
# function(tableObj, perc = 0.05, n = 1000) {
#   if(exists(".Random.seed", envir = .GlobalEnv)) {
#     save.seed <- get(".Random.seed", envir= .GlobalEnv)
#     on.exit(assign(".Random.seed", save.seed, envir = .GlobalEnv))
#   } else {
#     on.exit(rm(".Random.seed", envir = .GlobalEnv))
#   }
#   zs <- function(x) {
#     y <- c(scale(x))
#     y[is.nan(y)] <- 0
#     y
#   }
#   rowMeans(replicate(n, quantile(zs(sample(tableObj, replace=TRUE)), c(1,perc))))[-1]
# }

findRareValues <- function(dat, propThresh = 0.02, maxPerc = 0.5, colsToExclude = NULL) {
  coi <- setdiff(names(dat), colsToExclude)
  res <- vector('list', length(coi))
  for(i in seq_along(coi)) {
    var <- coi[i]
    val <- gsub('[[:space:]]', '', tolower(dat[[var]]))
#     to consider
#     val <- gsub('[^a-zA-Z0-9.]+', '*', val)
    val <- val[!is.na(val) & nchar(val) > 0]
    nv <- length(val)
    vtbl <- table(val)
    percThresh <- NA
    if(maxPerc < 0.05) {
      percThresh <- quantile(vtbl, maxPerc)
    } else {
      ps <- quantile(vtbl, c(seq(100)/100, maxPerc))
      ix <- which(ps < ps[101])
      if(length(ix)) {
        percThresh <- ps[max(ix)]
      }
    }
#     if(median(vtbl) > 1) {
    if(!is.na(percThresh)) {
      th <- max(1, floor(min(nv * propThresh, percThresh)))
      ix <- which(vtbl <= th)
      if(length(ix)) {
        ptbl <- proportions(vtbl)
        res[[i]] <- cbind(var, as.data.frame(vtbl[ix, drop = FALSE], stringsAsFactors = FALSE), Prop = c(ptbl[ix, drop = FALSE]))
      }
    }
  }
  z <- do.call(rbind, res)
  rownames(z) <- NULL
  z
}
