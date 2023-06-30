library(EHR)

mkdat <- function() {
  npat <- 3
  visits <- floor(runif(npat, min=2, max=6))
  id <- rep(1:npat, visits)
  dt_samp <- as.Date(sort(sample(700, sum(visits))), origin = '2019-01-01')
  tm_samp <- as.POSIXct(paste(dt_samp, '10:00:00'), tz = 'UTC')
  dt <- tm_samp + rnorm(sum(visits), 0, 1*60*60)
  dose_morn <- sample(c(2.5,5,7.5,10), sum(visits), replace = TRUE)
  conc <- round(rnorm(sum(visits), 1.5*dose_morn, 1),1)
  ld <- dt - sample(10:16, sum(visits), replace = TRUE) * 3600
  ld[rnorm(sum(visits)) < .3] <- NA
  age <- rep(sample(40:75, npat), visits)
  gender <- rep(sample(0:1, npat, replace=TRUE), visits)
  weight <- rep(round(rnorm(npat, 180, 20)),visits)
  hgb <- rep(rnorm(npat, 10, 2), visits)
  data.frame(id, dt, dose_morn, conc, ld, age, gender, weight, hgb)
}

set.seed(30)
dat <- mkdat()

pk1 <- run_Build_PK_Oral(x = dat,
                  idCol = "id",
                  dtCol = "dt",
                  doseCol = "dose_morn",
                  concCol = "conc",
                  ldCol = NULL,
                  first_interval_hours = 336,
                  doseFreq = 2,
                  imputeClosest = NULL
)

pk2 <- run_Build_PK_Oral(x = dat, doseCol = "dose_morn", ldCol = "ld", doseFreq = 3, imputeClosest = 'weight', date.tz = 'UTC')

expect_equivalent(c(0,8,8), colSums(is.na(pk1))[c('time','amt','dv')])
expect_equivalent(c(0,8,11), colSums(is.na(pk2))[c('time','amt','dv')])
