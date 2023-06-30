library(EHR)

suppress <- function(x) suppressMessages(suppressWarnings(x))

set.seed(6543)

build_date <- function(x) format(seq(x, length.out=5, by="1 hour"), "%Y-%m-%d %H:%M")
dates <- unlist(lapply(rep(Sys.time(),3), build_date))

plconc <- data.frame(mod_id = rep(1:3,each=5),
  mod_id_visit = rep(1:3,each=5)+0.1,
  event = rep(1:5,times=3),
  conc.level = 15*exp(-1*rep(1:5,times=3))+rnorm(15,0,0.1),
  date.time = as.POSIXct(dates)
)

ivdose <- data.frame(mod_id = 1:3,
  date.dose = substr(dates[seq(1,15,by=5)],1,10),
  infuse.time.real = NA, infuse.time = NA, infuse.dose = NA,
  bolus.time = as.POSIXct(dates[seq(1,15,by=5)])-300,
  bolus.dose = 90,
  maxint = 0L,
  weight = 45
)

doseData <- structure(list(id = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  dt = c("2020-01-01 08:00", "2020-01-02 08:00", "2020-01-02 20:00",
    "2020-01-02 20:00", "2020-01-03 08:00", "2020-01-05 08:00", "2020-01-05 08:00",
    "2020-01-05 08:00", "2020-01-05 20:00", "2020-01-06 08:00", "2020-01-06 08:00",
    "2020-01-06 20:00", "2020-01-07 20:00", "2020-01-07 20:00"),
  dose = c(30, 10, 20, 30, 10, 10, 20, 30, 10, 20, 10, 10, 20, 20),
  dur = c(3600, 7200, 1800, 3600, 3600, 7200, 1800, 3600, 7200, 1800, 1800, 1800, 1800, 3600)),
  class = "data.frame", row.names = c(NA, -14L)
)

dose <- run_MedStrIII(dose.path = doseData, dose.columns = list(id = 'id', datetime = 'dt', dose = 'dose', duration = 'dur'))
dose_rd <- dose
dose_rd[2:4,'rate'] <- dose_rd[2:4,'dose']

concData <- structure(
  list(
    id = c(1, 1, 1, 1, 1, 1),
    dt = c("2020-01-02 07:30", "2020-01-03 07:30", "2020-01-04 07:30", "2020-01-05 07:30", "2020-01-06 07:30", "2020-01-07 07:30"),
    level = c(0.524, 0.494, 0.059, 0.003, 0.384, 0.455)
  ),
  class = "data.frame", row.names = c(NA, -6L)
)

pk1 <- run_Build_PK_IV(conc = plconc,
  conc.columns = list(id = 'mod_id', datetime = 'date.time', druglevel = 'conc.level', idvisit = 'mod_id_visit'),
  dose = ivdose,
  dose.columns = list(id = 'mod_id', date = 'date.dose', bolusDatetime = 'bolus.time', bolusDose = 'bolus.dose', gap = 'maxint', weight = 'weight')
)

pk2 <- run_Build_PK_IV(conc = concData,
  conc.columns = list(id = 'id', datetime = 'dt', druglevel = 'level'),
  dose = dose,
  dose.columns = list(id = 'id', otherDatetime = 'date.time', otherDose = 'dose', otherRate = 'rate')
)

pk3 <- run_Build_PK_IV(conc = concData,
  conc.columns = list(id = 'id', datetime = 'dt', druglevel = 'level'),
  dose = dose_rd,
  dose.columns = list(id = 'id', otherDatetime = 'date.time', otherDose = 'dose', otherRate = 'rate'),
  doseFreq = 2
)

expect_true(all(plconc$conc.level - na.omit(pk1$dv) < 1e-8))
expect_equivalent(c(0,15,3), colSums(is.na(pk1))[c('time','amt','dv')])
expect_true(all(concData$level - na.omit(pk2$dv) < 1e-8))
expect_equivalent(c(0,6,5), colSums(is.na(pk2))[c('time','amt','dv')])
expect_true(all(concData$level - na.omit(pk3$dv) < 1e-8))
expect_equivalent(c(0,6,3), colSums(is.na(pk3))[c('time','amt','dv')])
expect_equivalent(c(1,8,0), na.omit(pk3$addl))

flow <- data.frame(mod_id=c(1,1,2,2,2),
                  mod_id_visit=c(46723,46723,84935,84935,84935),
                  record.date=c("07/05/2019 5:25","07/05/2019 6:01",
                                "09/04/2020 3:21", "09/04/2020 4:39",
                                "09/04/2020 5:32"),
                  Final.Weight=c(6.75,6.75,4.5,4.5,4.5),
                  Final.Rate=c(rep("1 mcg/kg/hr",2),
                                rep("0.5 mcg/kg/hr",3)),
                  Final.Units=c("3.375","6.5",
                                "2.25","2.25","2.25")
)
flow[,'unit'] <- sub('.*[ ]', '', flow[,'Final.Rate'])
flow[,'rate'] <- as.numeric(sub('([0-9.]+).*', '\\1', flow[,'Final.Rate']))

mar <- data.frame(mod_id=rep(1,5),
                  Date=rep("2019-07-05",5),
                  Time=c("07:12","07:31","08:47","09:16","10:22"),
                  `med:mDrug`=c("Fakedrug2","Fakedrug1","Fakedrug2",
                                "Fakedrug3","Fakedrug4"),
                  `med:dosage`=c("30 mg","0.5 mcg","1 mg",
                                "20 mg","3 mcg/kg/min"),
                  `med:route`=rep("IV",5),
                  `med:given`=rep("Given",5),
                  check.names=FALSE
)

dose1 <- suppress(run_MedStrI(mar.path = mar,
  mar.columns = list(id = 'mod_id', datetime = c('Date','Time'), dose = 'med:dosage', drug = 'med:mDrug', given = 'med:given'),
  flow.path = flow,
  flow.columns = list(id = 'mod_id', datetime = 'record.date', finalunits = 'Final.Units', unit = 'unit', rate = 'rate', weight = 'Final.Weight')
))
conc1 <- structure(
  list(
    id = c(1, 1, 2),
    dt = c("2019-07-04 12:30", "2019-07-05 07:45", "2020-09-04 07:30"),
    level = c(0.5, 0.4, 0.75)
  ),
  class = "data.frame", row.names = c(NA, -3L)
)
demo1 <- data.frame(id = c(1,2), sex = c('M','F'))
lab1 <- data.frame(id = 1, dt = c('2019-07-05 06:00', '2019-07-05 08:00'), bp = c('120/80','122/80'))
censor1 <- data.frame(id = 2, dt = '2020-09-01 00:00')
pk4 <- suppress(run_Build_PK_IV(conc = conc1,
  conc.columns = list(id = 'id', datetime = 'dt', druglevel = 'level'),
  dose = dose1,
  dose.columns = list(id = 'mod_id', infuseDatetime = 'infuse.time', infuseTimeExact = 'infuse.time.real', infuseDose = 'infuse.dose',
    bolusDatetime = 'bolus.time', bolusDose = 'bolus.dose', gap = 'maxint', weight = 'weight'
  ),
  demo.list = demo1, demo.columns = list(id = 'id'),
  lab.list = lab1, lab.columns = list(id = 'id', datetime = 'dt'),
  pk.vars = 'date'
))

pk5 <- suppress(run_Build_PK_IV(conc = conc1,
  conc.columns = list(id = 'id', datetime = 'dt', druglevel = 'level'),
  dose = dose1,
  dose.columns = list(id = 'mod_id', infuseDatetime = 'infuse.time', infuseTimeExact = 'infuse.time.real', infuseDose = 'infuse.dose',
    bolusDatetime = 'bolus.time', bolusDose = 'bolus.dose', gap = 'maxint', weight = 'weight'
  ),
  censor = censor1, censor.columns = list(id = 'id', datetime = 'dt'),
  demo.list = demo1, demo.columns = list(id = 'id'),
  lab.list = lab1, lab.columns = list(id = 'id', datetime = 'dt'),
  pk.vars = 'date'
))

expect_equivalent(c(0,2,3), colSums(is.na(pk4))[c('time','amt','dv')])
expect_equivalent(c(6.75,0,NA,2.25,NA), pk4$rate)
expect_equivalent(c(0,1,2), colSums(is.na(pk5))[c('time','amt','dv')])
expect_equivalent(c(6.75,0,NA), pk5$rate)
expect_equivalent(c('120/80','122/80','122/80'), na.omit(pk5$bp))

f1 <- system.file("examples", "iv_pk_raw_conc.csv", package = "EHR")
f2 <- system.file("examples", "iv_pk_raw_dose.csv", package = "EHR")
f3 <- system.file("examples", "iv_pk_output.csv", package = "EHR")
dat1 <- read.csv(f1)
dat2 <- read.csv(f2)

# doseFreq can be specified by II column
# dose and conc in same file?
ivout <- run_Build_PK_IV(conc = dat1,
  conc.columns = list(id = 'CID', datetime = 'DT', druglevel = 'DV'),
  dose = dat2,
  dose.columns = list(id = 'CID', otherDatetime = 'DT', otherDose = 'AMT', otherRate = 'RATE'),
  doseFreq = 2
)
# it should match this -- should dose after last conc be removed?
ans <- read.csv(f3, na.strings = '.')[,c('CID','TIME','AMT','DV','RATE','ADDL','II','EVID')]
expect_equivalent(ivout[,setdiff(names(ivout), 'mdv')], ans)

# f1 <- system.file("examples", "oral_pk_raw.csv", package = "EHR")
# f2 <- system.file("examples", "oral_pk_output.csv", package = "EHR")
# dat1 <- read.csv(f1)
# # dat1 <- dat1[c(1,3,4,6,7,10,13),]
# 
# # doseFreq can be specified by II column
# oralout <- run_Build_PK_Oral(dat1, idCol = 'CID', dtCol = 'DT', doseCol = "AMT", concCol = 'DV', doseFreq = 2)
# # it should match this
# ans <- read.csv(f2)[,c('CID','TIME','AMT','DV','EVID','ADDL','II')]
# # expect_equivalent(oralout[,setdiff(names(oralout), 'mdv')], ans)
