library(EHR)

suppress <- function(x) suppressMessages(suppressWarnings(x))

f1 <- system.file("examples", "iv_pk_raw_dose.csv", package = "EHR")
f2 <- system.file("examples", "iv_pk_raw_conc.csv", package = "EHR")
f3 <- system.file("examples", "generic_pk_raw.csv", package = "EHR")
f4 <- system.file("examples", "generic_pk_rawII.csv", package = "EHR")
f5 <- system.file("examples", "iv_pk_output.csv", package = "EHR")
dat1 <- read.csv(f1) # dose file
dat2 <- read.csv(f2) # conc file
dat3 <- read.csv(f3) # dose+conc; use doseFreq for repeated dosing
dat4 <- read.csv(f4) # dose+conc with II for repeating dosing
# it should match this
ans <- read.csv(f5, na.strings = '.')[,c('CID','TIME','AMT','DV','RATE','ADDL','II','EVID')]

o1 <- suppress(run_Build_PK(
  dose = dat1,
  dose.columns = list(id = 'CID', datetime = 'DT', dose = 'AMT', rate = 'RATE'),
  conc = dat2,
  conc.columns = list(id = 'CID', datetime = 'DT', druglevel = 'DV'),
  doseFreq = 2
))
o3nr <- suppress(run_Build_PK(dose = dat3,
  dose.columns = list(id = 'CID', datetime = 'DT', dose = 'AMT', rate = 'RATE', druglevel = 'DV')
))
o3rd <- suppress(run_Build_PK(dose = dat3,
  dose.columns = list(id = 'CID', datetime = 'DT', dose = 'AMT', rate = 'RATE', druglevel = 'DV'),
  doseFreq = 2
))
o4 <- suppress(run_Build_PK(dose = dat4,
  dose.columns = list(id = 'CID', datetime = 'DT', dose = 'AMT', rate = 'RATE', druglevel = 'DV', II = 'II')
))

expect_equivalent(o1[,setdiff(names(o1), c('mdv','date'))], ans)
expect_equivalent(o3rd[,setdiff(names(o3rd), c('mdv','date'))], ans)
expect_equivalent(o4[,setdiff(names(o4), c('mdv','date'))], ans)
# o3nr (non-repeating) should basically match its input
dat3nr <- dat3[,c('CID','DT','AMT','DV','RATE')]
dat3_dt <- as.POSIXct(dat3nr[,'DT'])
dat3nr[,'DT'] <- round(as.numeric(difftime(dat3_dt, dat3_dt[1], unit = 'hour')), 2)
expect_equivalent(o3nr[,setdiff(names(o3nr), c('mdv','date','evid'))], dat3nr[-14,])

f11 <- system.file("examples", "oral_pk_raw.csv", package = "EHR")
f12 <- system.file("examples", "oral_pk_rawII.csv", package = "EHR")
f13 <- system.file("examples", "oral_pk_output.csv", package = "EHR")
dat11 <- read.csv(f11)
dat12 <- read.csv(f12)
dat13 <- read.csv(f13)[,c('CID','TIME','AMT','DV','ADDL','II','EVID')]
o11 <- suppress(run_Build_PK(dose = dat11,
                    dose.columns = list(id = 'CID', datetime = 'DT', dose = 'AMT', druglevel = 'DV'),
                    doseFreq = 2
))
o12 <- suppress(run_Build_PK(dose = dat12,
                    dose.columns = list(id = 'CID', datetime = 'DT', dose = 'AMT', druglevel = 'DV', II = 'II')
))

expect_equivalent(o11[,setdiff(names(o11), c('mdv','date','rate'))], dat13)
expect_equivalent(o11[,setdiff(names(o12), c('mdv','date','rate'))], dat13)
