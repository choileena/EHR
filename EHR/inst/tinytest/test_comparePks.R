library(EHR)

suppress <- function(x) suppressMessages(suppressWarnings(x))

set.seed(4)

mkConc <- function(npat = 10) {
  visits <- floor(runif(npat, min=2, max=6))
  id <- rep(1:npat, visits)
  dt_samp <- as.Date(sort(sample(700, npat)), origin = '2019-01-01')
  tm_samp <- as.POSIXct(paste(dt_samp, '10:00:00'), tz = 'America/Chicago')
  # add 0-14 days and ~center~ 10am
  dt <- rep(tm_samp, visits) + floor(runif(sum(visits), 0, 14)) * 86400 + rnorm(sum(visits), 0, 1*60*60)
  conc <- round(rnorm(sum(visits), 4), 2)
  out <- data.frame(id, dt, conc)[order(id,dt),]
  rownames(out) <- NULL
  out
}

mkDose <- function(baseline_dates) {
  uid <- sort(as.numeric(names(baseline_dates)))
  npat <- length(uid)
  visits <- floor(runif(npat, min=20, max=40))
  d0 <- baseline_dates + rnorm(npat, 0, 1*60*60)
  d1 <- as.POSIXct(unname(unlist(mapply(seq, d0, length.out = visits, by = 10*60*60))))
  dt <- d1 + rnorm(sum(visits), 0, 1*60*60)
  id <- rep(uid, visits)
  pm <- matrix(c(0.8, 0.1, 0.1, 0.3, 0.4, 0.3, 0.4, 0.4, 0.2), 3, 3, byrow = TRUE)
  upm <- pm[sample(3, npat, replace = TRUE),]
  dose <- unlist(lapply(seq(npat), function(i) {
    sample(c(5,2,1), visits[i], replace = TRUE, upm[i,])
  }))
  data.frame(id, dt, dose)
}

mkLabs <- function(baseline_dates) {
  uid <- sort(as.numeric(names(baseline_dates)))
  npat <- length(uid)
  visits <- floor(runif(npat, min=0, max=12))
  nv <- sum(visits)
  w0 <- runif(npat, 100, 250)
  do.call(rbind, lapply(seq(npat), function(i) {
    v_i <- visits[i]
    if(v_i == 0) return(NULL)
    dt <- sort(unname(baseline_dates[i]) + runif(v_i, -300*86400, 300*86400))
    data.frame(
      id = uid[i],
      dt = round(dt, 'mins'),
      weight = round(rnorm(v_i, w0[i], 5)),
      hgb = round(rnorm(v_i, 10, 2),2),
      scr = round(rnorm(v_i, 1, 0.1),2)
    )
  }))
}

mkDemo <- function(npat = 10) {
  id <- seq(npat)
  age <- sample(40:75, npat)
  gender <- sample(0:1, npat, replace=TRUE)
  data.frame(id, age, gender)
}

conc_dat <- mkConc()
bd <- as.POSIXct(tapply(conc_dat$dt, conc_dat$id, min))
dose_dat <- mkDose(bd)
dose_dat$rate <- NA
labs_dat <- mkLabs(bd)
demo_dat <- mkDemo(length(bd))

# pass to PK IV
suppress(o0 <- run_Build_PK_IV(
  conc_dat, conc.columns = list(id = 'id', datetime = 'dt', druglevel = 'conc'),
  dose_dat, dose.columns = list(id = 'id', otherDatetime = 'dt', otherDose = 'dose', otherRate = 'rate'),
  demo.list = demo_dat, demo.columns = list(id = 'id'),
  lab.list = labs_dat, lab.columns = list(id = 'id', datetime = 'dt'),
  doseFreq = 2, date.format = "%Y-%m-%d %H:%M:%S", labPriorWindow = 90, pk.vars = 'date'
))

# pass to PK generic
suppress(o1 <- run_Build_PK(dose_dat, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose'),
  conc_dat, conc.columns = list(id = 'id', datetime = 'dt', druglevel = 'conc'), doseFreq = 2, date.format = "%Y-%m-%d %H:%M:%S"
))
o2 <- add_Labs(o1, list(id = 'id', datetime = 'date'), labs_dat, list(id = 'id', datetime = 'dt'), 90)
o3 <- add_Demo(o2, list(id = 'id'), demo_dat, list(id = 'id'))

expect_equal(o0, o3[,names(o0)])
